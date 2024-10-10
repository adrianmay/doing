{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Ord (comparing)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Time.ISO8601
import Data.Time.LocalTime
import Debug.Trace
import Text.Printf
import qualified Data.Map.Strict as M

data Trans = Trans
  { tAct    :: String 
  , tMoment :: LocalTime
  , tDesc   :: String
  }

data FromTo = FromTo
  { fAct      :: String
  , fInterval :: (LocalTime, LocalTime)
  , fDesc     :: String 
  }

data Stint = Stint 
  { sMinutes :: Int
  , sInterval :: (LocalTime, LocalTime)
  , sDesc     :: String
  } 

main = do
  l :: [String] <- lines <$> getContents 
  let ps      :: [Trans]
               = parseTransition <$> l
      fts     :: [FromTo]
               = toFromTos ps
      byMonth :: Map (Year, MonthOfYear) [FromTo]
               = toMapBy fromToToYearMonth id fts
      byAct   :: Map (Year, MonthOfYear) (Map String [Stint])
               = toMapBy fAct mkStint <$> byMonth
      withTot :: Map (Year, MonthOfYear) (Map String (Int, [Stint]))
               = M.map (addUp sMinutes) <$> byAct -- broken laziness but who cares - the data is small
  printAll withTot
  putStrLn ""

parseTransition :: String -> Trans
parseTransition s =
  let l = splitWhen (=='\t') s
   in if length l == 2 || length l == 3
      then
         let t :: LocalTime = roundLocalTime $ fromJust $ iso8601ParseM (takeWhile (/='+') (l!!1)) -- FIXME fromJust
          in Trans (l!!0) t (if length l == 3 then l!!2 else "")
      else error "Bad raw input in transition"

roundLocalTime (LocalTime d (TimeOfDay h m s)) = LocalTime d (TimeOfDay h m 0)

toFromTos :: [Trans] -> [FromTo]
toFromTos ts = 
  let z = zip ts (drop 1 ts)
   in (\(Trans a1 t1 _, Trans _ t2 desc) -> (FromTo a1 (t1, t2) desc) ) <$> z

fromToToYearMonth :: FromTo -> (Year, MonthOfYear)
fromToToYearMonth (FromTo _ (f,t) _) = localTimeToYearMonth f

localTimeToYearMonth :: LocalTime -> (Year, MonthOfYear)
localTimeToYearMonth t = (\(y,m,_)->(y,m)) $ toGregorian $ localDay t

mkStint :: FromTo -> Stint
mkStint (FromTo _ (f,t) d) =
  Stint ((`div` 60) $ floor $ nominalDiffTimeToSeconds $ diffLocalTime t f) (f,t)
        d

toMapBy :: forall a k v. Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v] -- slow but a small collection usually (cos transistions gets cleaned up regularly)
toMapBy key val as = foldr ( \a mp -> M.insertWith (++) (key a) [val a] mp) M.empty as -- foldr is questionable, but likewise about the sizes, plus it preserves the order

addUp :: (a -> Int) -> [a] -> (Int, [a])
addUp f ss = let t = foldl' (\tot a -> tot + f a) 0 ss in (t,ss)


printAll :: Map (Year, MonthOfYear) (Map String (Int, [Stint])) -> IO ()
printAll mp =  mapM_ (uncurry printMonth) $ sortBy (comparing fst) $ M.toList mp

printMonth :: (Year, MonthOfYear) ->  Map String (Int, [Stint]) -> IO ()
printMonth (y,m) mp = 
  let ams  :: [(String, (Int, [Stint]))] = M.toList mp
      nzms  = filter ((/="0") . fst) ams
      bms   = filter ((/='_') . (!!0) . fst) nzms
      tot  :: Int = fst $ addUp fst $ snd <$> bms
  in do
    putStrLn "-----------------------------------------"
    putStrLn $ snd ((months defaultTimeLocale)!!(m-1)) <> " " <> show y <> ": " <> showDurationsLong tot
    putStrLn "-----------------------------------------"
    mapM_ printAct $ sortBy (comparing (\(k,_)-> (k!!0)=='_')) nzms

printAct :: (String, (Int, [Stint])) -> IO ()
printAct (act_,(tot,stints)) = do
  let act = if (act_!!0) == '_' then drop 1 act_ <> " (unbillable)" else act_
  putStrLn ""
  putStrLn (act <> ": " <> showDurationsLong tot)
  mapM_ printStint stints 

printStint :: Stint -> IO ()
printStint (Stint dur (f,t) desc) = 
 let (fd,ft) = (localDay f, localTimeOfDay f) 
     (_ ,tt) = (localDay t, localTimeOfDay t) 
  in putStrLn $ "   " <> showDate fd <> "  |  " <> showTimeShort ft <> " -> " <> showTimeShort tt <> " = " <> showDurationShort dur <> "  |  " <> desc
      
showDate = formatTime defaultTimeLocale "%a %e"      
showTimeShort = formatTime defaultTimeLocale "%H:%M"      

showDurationsLong :: Int -> String
showDurationsLong minutes = show (minutes `div` 60) <> " hours and " <> show (minutes `mod` 60) <> " minutes"

showDurationShort :: Int -> String
showDurationShort minutes = printf "%02d" (minutes `div` 60) <> ":" <> printf "%02d" (minutes `mod` 60)
  
