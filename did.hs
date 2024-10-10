{-# LANGUAGE ScopedTypeVariables #-}

import Data.Time.LocalTime
import Text.Printf
import Control.Arrow
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time.Format
import Data.Time.ISO8601
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)

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

localTimeToYearMonth :: LocalTime -> (Year, MonthOfYear)
localTimeToYearMonth t = (\(y,m,_)->(y,m)) $ toGregorian $ localDay t

fromToToYearMonth :: FromTo -> (Year, MonthOfYear)
fromToToYearMonth (FromTo _ (f,t) _) = 
  let fym = localTimeToYearMonth f
      tym = localTimeToYearMonth t
   in if fym /= tym then error "Stint over month change" else fym    

mkStint :: FromTo -> Stint
mkStint (FromTo _ (f,t) d) =
  Stint ((`div` 60) $ floor $ nominalDiffTimeToSeconds $ diffLocalTime t f)
        (f,t)
        d

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
               = M.map addUp <$> byAct -- broken laziness but who cares - the data is small
  printAll withTot

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

toMapBy :: forall a k v. Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v] -- slow but a small collection usually (cos transistions gets cleaned up regularly)
toMapBy key val as = foldr ( \a mp -> M.insertWith (++) (key a) [val a] mp) M.empty as -- foldr is questionable, but likewise about the sizes, plus it preserves the order

addUp :: [Stint] -> (Int, [Stint])
addUp ss = let t = foldl' (\tot (Stint d _ _) -> tot+d) 0 ss in (t,ss)

printAll :: Map (Year, MonthOfYear) (Map String (Int, [Stint])) -> IO ()
printAll mp = sequence_ $ M.mapWithKey printMonth mp

printMonth :: (Year, MonthOfYear) ->  Map String (Int, [Stint]) -> IO ()
printMonth (y,m) mp = do
  putStrLn $ "======= Month: " <> snd ((months defaultTimeLocale)!!(m-1)) <> " " <> show y <> "  ======"
  sequence_ $ M.mapWithKey printAct mp

printAct :: String -> (Int, [Stint]) -> IO ()
printAct act (tot,stints) = do
  if act=="0" then pure () else do
    putStrLn ("------  Project: " <> act <> " (" <> myFormatDiffTimeLong tot <> ")  ------")
    mapM_ printStint stints 
    putStrLn ""

printStint :: Stint -> IO ()
printStint (Stint dur (f,t) desc) = 
 let (fd,ft) = (localDay f, localTimeOfDay f) 
     (td,tt) = (localDay t, localTimeOfDay t) 
  in if fd /= td then error "Overnight stint" else do
    putStrLn $ showDate fd <> "  |  " <> showTimeShort ft <> " -> " <> showTimeShort tt <> " = " <> myFormatDiffTimeShort dur <> "  |  " <> desc
      
showDate = formatTime defaultTimeLocale "%a %e"      
showTimeShort = formatTime defaultTimeLocale "%H:%M"      

myFormatDiffTimeLong :: Int -> String
myFormatDiffTimeLong minutes = show (minutes `div` 60) <> " hours and " <> show (minutes `mod` 60) <> " minutes"

myFormatDiffTimeShort :: Int -> String
myFormatDiffTimeShort minutes = printf "%02d" (minutes `div` 60) <> ":" <> printf "%02d" (minutes `mod` 60)
  

