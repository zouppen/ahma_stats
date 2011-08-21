{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad (when)
import Data.DateTime
import Data.Maybe (fromJust, isNothing)
import Data.Time.LocalTime (getCurrentTimeZone, timeZoneOffsetString)
import System.Console.CmdArgs.Implicit
import Text.JSON
import Text.Printf (PrintfType,printf)

import TimeQuirks
import DbHelpers
import Extractor

data Args = Args { database :: String
                 , when_    :: Maybe String
                 , timezone :: String
                 } deriving (Show, Data, Typeable)

-- TODO add support for times, too!

synopsis tz =
  Args{ database = defDB &= typ "URL" &= help
                   ("CouchDB database URL (default: " ++ defDB ++ ")")
      , when_ = def &= typ "DATE" &= help
                "Time of measurement (default: newest)"
      , timezone = tz &= typ "TZ" &= help 
                   ("Timezone (default: " ++ tz ++ ")")
      }
  &= program "stats"
  &= summary "Ahma ask 0.1"
  &= help "Queries a single measurement from a database."

-- |Converts user-supplied parameters to a format suitable query. URL
-- must be parsed, dates must be in UTC format etc.
convertArgs :: Args -> QueryOne
convertArgs a = QueryOne { qoDatabase = toURI $ database a
                         , qoWhen = when_ a >>= Just . tzParse
                         }
  where tzParse = parseWisely $ zoneParse' (timezone a)

-- |Main function and printing.
main = do 
  tzHere <- getCurrentTimeZone
  args <- cmdArgs $ synopsis $ timeZoneOffsetString tzHere
  mbStats <- queryOne $ convertArgs args
  when (isNothing mbStats) $ fail "No matching results"
  let Just stats = mbStats
  let tz = zoneParse' $ timezone args
  let Ok ts = timeFromObj "timestamp" stats
  printf "Measured at %s (time zone %s)\n" (showTime tz ts) (show tz)
  printTemp "box" stats
  printTemp "in" stats
  printTemp "north" stats
  printTemp "south" stats

printTemp :: (PrintfType b) => String -> JSObject JSValue -> b
printTemp field js = do
  case valFromObj ("temp_"++field) js of
    Ok a -> printf formatOk field (a::Double)
    Error _ -> printf formatMissing field $ getError ("temp_"++field) js
  where
    formatOk = "%-6s%+6.1f °C\n"
    formatMissing = "%-6s (missing, %s)\n"

getError :: String -> JSObject JSValue -> String
getError field js =
  case valFromObj field errors of
    Ok a -> dropLast $ (lines a) !! 2
    Error _ -> "the value is not measured"
  where
    Ok errors = valFromObj "errors" js
    dropLast a = take (length a - 1) a

timeFromObj :: String -> JSObject JSValue -> Result DateTime
timeFromObj field js =
  case valFromObj field js of
    Ok a -> Ok $ fromSeconds a
    Error e -> Error e -- Different type of Result
