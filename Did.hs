{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad (when)
import Control.Arrow ((&&&), second)
import Data.Bool (bool)
import Data.Function (on, (&))
import Data.List (foldl', sortBy, groupBy, intersperse)
import Data.List.Split (splitWhen)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Ord (comparing)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekCalendar, FirstWeekType(..), DayOfWeek(..))
import Data.Time.Clock (nominalDiffTimeToSeconds, UTCTime(..))
import Data.Time.Format (months, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..), diffLocalTime, getTimeZone, TimeZone(..))
import System.Environment (getArgs)
import Text.Printf (printf)
import qualified Data.Map.Strict as M

-- Year, Month, Day of Month or
-- Year, Week, Day of Week
type HumanDate = (Integer, Int, Int)

data Trans = Trans
  { tAct    :: String 
  , tMoment :: LocalTime
  , tDesc   :: String
  }

data FromTo = FromTo
  { fAct      :: String
  , fInterval :: (LocalTime, LocalTime)
  , fDesc     :: String 
  } deriving Show

data Stint = Stint 
  { sMinutes :: Int
  , sInterval :: (LocalTime, LocalTime)
  , sDesc     :: String
  , sCalday   :: Int -- day of month or of week
  } 

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["m"] -> moreMain False
    ["w"] -> moreMain True
    _ -> putStrLn "Call with m or w as command line parameter"

moreMain :: Bool -> IO ()
moreMain weekly = do
  l :: [String] <- lines <$> getContents 
  let ps        :: [Trans]
                 = parseTransition <$> l
      fts       :: [FromTo]
                 = toFromTos ps
      period    :: FromTo -> HumanDate           
                 = weekly & bool fromToToYearMonth fromToToYearWeek
      withPeriod :: [(HumanDate,FromTo)]
                 = (period &&& id) <$> fts
      byPeriod   :: [((Integer, Int), [(Int,FromTo)])] -- (y,m),[(day,ft)]
                 = groupInto (fstsnd . fst) ( \(hd,ft) -> (thd3 hd,ft)) withPeriod
  when weekly $ do
    putStrLn "======================================================"
    putStrLn ""
    putStrLn " Tot     Mon    Tue    Wed    Thu    Fri    Sat    Sun"
  mapM_ (uncurry (weekly & bool doMonth doWeek)) byPeriod
  putStrLn ""


parseTransition :: String -> Trans
parseTransition s =
  let l = splitWhen (=='\t') s
   in if length l == 2 || length l == 3
      then
         let t :: LocalTime = roundLocalTime $ fromMaybe (error $ "Can't parse date: " <> (l!!0)) $ iso8601ParseM (takeWhile (/='+') (l!!0))
          in Trans (l!!1) t (if length l == 3 then l!!2 else "")
      else error $ "Bad raw input in transition: " <> s

roundLocalTime (LocalTime d (TimeOfDay h m s)) = LocalTime d (TimeOfDay h m 0)

toFromTos :: [Trans] -> [FromTo]
toFromTos ts = 
  let z = zip ts (drop 1 ts)
   in (\(Trans a1 t1 _, Trans _ t2 desc) -> (FromTo a1 (t1, t2) desc) ) <$> z

fromToToYearMonthOrWeek :: (LocalTime -> HumanDate) -> FromTo -> HumanDate
fromToToYearMonthOrWeek mow (FromTo _ (f,t) _) = mow f

localTimeToYearMonth :: LocalTime -> HumanDate
localTimeToYearMonth t = toGregorian $ localDay t

localTimeToYearWeek :: LocalTime -> HumanDate
localTimeToYearWeek t = (third fromEnum) $ toWeekCalendar FirstMostWeek Monday $ localDay t

fromToToYearMonth  :: FromTo -> HumanDate
fromToToYearMonth = fromToToYearMonthOrWeek localTimeToYearMonth 

fromToToYearWeek  :: FromTo -> HumanDate
fromToToYearWeek = fromToToYearMonthOrWeek localTimeToYearWeek 

groupInto :: forall a k v. Eq k => (a -> k) -> (a -> v) -> [a] -> [(k,[v])] -- assuming already ordered by k
groupInto key val as =
  let kvs :: [(k,v)]= (key &&& val) <$> as 
      gd  :: [[(k,v)]] = groupBy ((==) `on` fst) kvs
   in map (fst . head &&& map snd) gd   
      

doMonth :: (Integer, Int) -> [(Int,FromTo)] -> IO ()  -- (y,m) , [(day,ft)]
doMonth (y,m) (dfts) = 
  let byAct :: Map String [Stint]
             = toMapBy (fAct . snd) mkStint dfts
      withTot :: Map String (Int, [Stint])       
               = M.map (\l -> ((foldl' (+) 0 (sMinutes <$> l)), l)) byAct
   in printMonth (y,m) withTot
  
doWeek :: (Integer, Int) -> [(Int,FromTo)] -> IO ()
doWeek ym alldfts =
  let dfts      :: [(Int,FromTo)]  = filter ( (/="0") . fAct . snd ) alldfts
      sts       :: [Stint]         = mkStint <$> dfts
      days_mins :: [(Int, Int)]    = (\(Stint m _ _ d) -> (d, m)) <$> sts
      gb        :: [(Int, [Int])]  = groupInto fst snd days_mins
      dts       :: [(Int, Int)]    = second (foldl' (+) 0) <$> gb
      tot       :: Int             = foldl' (+) 0 (snd <$> dts)
      z         :: [((Int, Int),(Int, Int))] = (zip ((0,0):dts) dts)
      gaps      :: [(Int,Int)]     = ( \ ((da,ma),(db,mb)) -> ((db-da-1)*7+2,mb) ) <$> z
      str       :: String = gaps >>= ( \(spaces,m) -> replicate spaces ' ' <> (showDurationShort m))
   in putStrLn (showDurationShort tot <> ":" <> str)

mkStint :: (Int,FromTo) -> Stint
mkStint (wd, FromTo _ (f,t) d) = Stint ((`div` 60) $ floor $ nominalDiffTimeToSeconds $ diffLocalTime t f) (f,t) d wd

toMapBy :: forall a k v. Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
toMapBy key val as = foldl' ( \mp a -> M.insertWith (flip (++)) (key a) [val a] mp) M.empty as

printMonth :: (Integer, Int) -> Map String (Int, [Stint]) -> IO ()
printMonth (y,m) mp = -- (y,m) (Map act (tot, [stint]))
  let ams  :: [(String, (Int, [Stint]))] = M.toList mp
      nzms  = filter ((/="0") . fst) ams
      bms   = filter ((/='_') . (!!0) . fst) nzms
      tot  :: Int = foldl' (+) 0 $ (fst . snd) <$> bms
  in do
    putStrLn ""
    putStrLn "-----------------------------------------"
    putStrLn $ snd ((months defaultTimeLocale)!!(m-1)) <> " " <> show y <> ": " <> showDurationsLong tot
    putStrLn "-----------------------------------------"
    mapM_ printActInMonth $ sortBy (comparing (\(k,_)-> (k!!0)=='_')) nzms

printActInMonth :: (String, (Int, [Stint])) -> IO ()
printActInMonth (act_,(tot,stints)) = do
  let act = if (act_!!0) == '_' then drop 1 act_ <> " (unbillable)" else act_
  putStrLn ""
  putStrLn (act <> ": " <> showDurationsLong tot)
  printStintsInMonth stints

      -- width = (length stints) * 13 - 2 -- For stint times column

decimalDuration :: Int -> Float
decimalDuration minutes = thd3 $ roundDuration minutes

roundDuration :: Int -> (Int, Int, Float)
roundDuration minutes = 
  let h = minutes `div` 60
      m = minutes `mod` 60
      f :: Float = fromIntegral (h*100 + round (((fromIntegral m :: Float) * 100) / 60)) / 100
   in (h,m,f)   

showDurationsLong :: Int -> String
showDurationsLong minutes = 
  let (h,m,f) = roundDuration minutes
   in show h <> " hours and " <> show m <> " minutes (" <> show f <> " hours)"

stintsByDay :: [Stint] -> Map Day [Stint]
stintsByDay stints =
  let withDays :: [(Day, Stint)] = ( \st -> (localDay $ fst $ sInterval st, st) ) <$> stints
   in toMapBy fst snd withDays

printStintsInMonth :: [Stint] -> IO ()  
printStintsInMonth stints = do -- mapM_ printStint stints 
  let byDay :: Map Day [Stint] = stintsByDay stints
      dayWidths :: [Int] = map (\l -> (numDisjointStints l) * 13 - 2) (M.elems byDay)
      width :: Int = maximum dayWidths
  sequence_ $ M.mapWithKey (printDaysStints width) byDay
  
printDaysStints :: Int -> Day -> [Stint] -> IO ()
printDaysStints width d sts = do
  tz <- getTimeZone (UTCTime d $ fromInteger (12*60*60))
  let daytot = foldl (+) 0 $ map sMinutes sts
      line = "   " <> showDate d <> " | " <> showDurationShort daytot <> " | " <> stintsPhrase width (timeZoneName tz) sts
  putStrLn line

stintsPhrase :: Int -> String -> [Stint] -> String
stintsPhrase width tz ss = 
  let descs = showDescs $ sDesc <$> ss
      fts = showIntervals $ sInterval <$> ss
   in fts <> " " <> tz <> (replicate (width - length fts) ' ') <> " | " <> descs

showIntervals :: [(LocalTime, LocalTime)] -> String
showIntervals = showIntervals_ . joinIf intervalsTouch

intervalsTouch :: (LocalTime, LocalTime) -> (LocalTime, LocalTime) -> Maybe (LocalTime, LocalTime)
intervalsTouch (f1,t1) (f2,t2) = if t1==f2 then Just (f1,t2) else Nothing

numDisjointStints :: [Stint] -> Int
numDisjointStints [] = 0
numDisjointStints [_] = 1
numDisjointStints (s1:s2:ss) = if isJust (intervalsTouch (sInterval s1) (sInterval s2))
                               then numDisjointStints (s2:ss)
                               else 1+numDisjointStints (s2:ss)



joinIf :: (a -> a -> Maybe a) -> [a] -> [a]
joinIf f [] = []
joinIf f [a] = [a]
joinIf f (a1:a2:as) = f a1 a2 & maybe (a1:joinIf f (a2:as)) (\aa -> joinIf f (aa:as))

showIntervals_ :: [(LocalTime, LocalTime)] -> String
showIntervals_ l = concat $ intersperse ", " (showInterval <$> l)

showDescs  :: [String] -> String
showDescs = concat . map (<> ". ") . filter (not . null)

showInterval :: (LocalTime, LocalTime) -> String
showInterval (st, en) = showTimeShort (localTimeOfDay st) <> "-" <> showTimeShort (localTimeOfDay en)  

showDate :: Day -> String      
showDate = formatTime defaultTimeLocale "%a %e"      

showTimeShort :: TimeOfDay -> String 
showTimeShort = formatTime defaultTimeLocale "%H:%M"      

showDurationShort :: Int -> String
showDurationShort minutes = printf "%02d" (minutes `div` 60) <> ":" <> printf "%02d" (minutes `mod` 60)
  
third :: (c -> d) -> (a,b,c) -> (a,b,d)
third f (a,b,c) = (a,b,f c)

fstsnd :: (a,b,c) -> (a,b)
fstsnd (a,b,_) = (a,b)

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

