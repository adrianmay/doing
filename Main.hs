{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Arrow ((&&&))
import Data.Char 
import Data.Function (on)
import Data.List (foldl', sortBy, groupBy)
import Data.List.Split (splitWhen)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Format (months, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), diffLocalTime)
import Text.Printf (printf)
import qualified Data.Map.Strict as M

type MonthOfYear = Int
type Year = Integer

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
  let ps        :: [Trans]
                 = parseTransition <$> l
      fts       :: [FromTo]
                 = toFromTos ps
      withMonth :: [((Year, MonthOfYear),FromTo)]
                 = (fromToToYearMonth &&& id) <$> fts
      byMonth   :: [  ((Year, MonthOfYear), [FromTo]) ]
                 = groupInto fst snd withMonth
  mapM_ (uncurry doMonth) byMonth
  putStrLn ""


parseTransition :: String -> Trans
parseTransition s =
  let l = splitWhen (=='\t') s
   in if length l == 2 || length l == 3
      then
         let t :: LocalTime = roundLocalTime $ fromMaybe (error $ "Can't parse date: " <> (l!!1)) $ iso8601ParseM (takeWhile (/='+') (l!!1))
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

groupInto :: forall a k v. Eq k => (a -> k) -> (a -> v) -> [a] -> [(k,[v])] -- assuming already ordered by k
groupInto key val as =
  let kvs :: [(k,v)]= (key &&& val) <$> as 
      gd  :: [[(k,v)]] = groupBy ((==) `on` fst) kvs
   in map (fst . head &&& map snd) gd   
      

doMonth :: (Year, MonthOfYear) -> [FromTo] -> IO ()
doMonth (y,m) fts = 
  let byAct :: Map String [Stint]
             = toMapBy fAct mkStint fts
      withTot :: Map String (Int, [Stint])       
               = M.map (\l -> ((foldl' (+) 0 (sMinutes <$> l)), l)) byAct
   in printMonth (y,m) withTot
  

mkStint :: FromTo -> Stint
mkStint (FromTo _ (f,t) d) = Stint ((`div` 60) $ floor $ nominalDiffTimeToSeconds $ diffLocalTime t f) (f,t) d

toMapBy :: forall a k v. Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
toMapBy key val as = foldl' ( \mp a -> M.insertWith (flip (++)) (key a) [val a] mp) M.empty as

printMonth :: (Year, MonthOfYear) -> Map String (Int, [Stint]) -> IO ()
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

addUp :: (a -> Int) -> [a] -> (Int, [a])
addUp f ss = let t = foldl' (\tot a -> tot + f a) 0 ss in (t,ss)

printAct :: (String, (Int, [Stint])) -> IO ()
printAct (act_,(tot,stints)) = do
  let act = if (act_!!0) == '_' then drop 1 act_ <> " (unbillable)" else act_
  putStrLn ""
  putStrLn (act <> ": " <> showDurationsLong tot)
  mapM_ printStint stints 

showDurationsLong :: Int -> String
showDurationsLong minutes = 
  let h = minutes `div` 60
      m = minutes `mod` 60
      f :: Float = fromIntegral (h*100 + round (((fromIntegral m :: Float) * 100) / 60)) / 100
   in show h <> " hours and " <> show m <> " minutes (" <> show f <> " hours)"

printStint :: Stint -> IO ()
printStint (Stint dur (f,t) desc) = 
 let (fd,ft) = (localDay f, localTimeOfDay f) 
     (_ ,tt) = (localDay t, localTimeOfDay t) 
  in putStrLn $ "   " <> showDate fd <> "  |  " <> showTimeShort ft <> " -> " <> showTimeShort tt <> " = " <> showDurationShort dur <> "  |  " <> desc
      
showDate :: Day -> String      
showDate = formatTime defaultTimeLocale "%a %e"      

showTimeShort :: TimeOfDay -> String 
showTimeShort = formatTime defaultTimeLocale "%H:%M"      

showDurationShort :: Int -> String
showDurationShort minutes = printf "%02d" (minutes `div` 60) <> ":" <> printf "%02d" (minutes `mod` 60)
  
