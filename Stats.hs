{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Maybe (fromJust)
import Data.Time.LocalTime (getCurrentTimeZone, timeZoneOffsetString)
import Network.URI
import System.Console.CmdArgs.Implicit
import Text.Printf (printf)
import TimeQuirks
import Extractor

data Args = Args { database :: String
                 , start :: Maybe String
                 , end :: Maybe String
                 , timezone :: String
                 } deriving (Show, Data, Typeable)

synopsis tz =
  Args{ database = defDB &= typ "URL" &= help
                   ("CouchDB database URL (default: " ++ defDB ++ ")")
      , start = def &= typ "DATE" &= help "Starting date (optional)"
      , end = def &= typ "DATE" &= help "Ending date (optional)"
      , timezone = tz &= typ "TZ" &= help 
                   ("Timezone (default: " ++ tz ++ ")")
      }
  &= program "stats"
  &= summary "Ahma stats 0.1"
  &= help "Extracts measurements from a database."

-- |Converts user-supplied parameters to a format suitable query. URL
-- must be parsed, dates must be in UTC format etc.
convertArgs :: Args -> Query
convertArgs a = Query { qDatabase = fromJust $ parseURI (database a)
                      , qStart = start a >>= Just . (parseWisely tz)
                      , qEnd = end a >>= Just . (parseWisely tz)
                      }
  where tz = zoneParse' (timezone a)

-- |Main function and printing.
main = do 
  tz <- getCurrentTimeZone
  args <- cmdArgs $ synopsis $ timeZoneOffsetString tz
  stats <- queryStats $ convertArgs args
  putStrLn $ "Time zone is " ++ (timezone args)
  mapM_ (pretty (read $ timezone args)) $ fromJust stats

pretty tz (name,s) = printf format
                     name
                     (minTemp s) (showTime tz $ minTime s)
                     (maxTemp s) (showTime tz $ maxTime s)
                     (avg s)
                     (n s)

format = unlines [ "Location: %s"
                 , "  minimum:%+6.1f °C at %s"
                 , "  maximum:%+6.1f °C at %s"
                 , "  average:%+6.1f °C"
                 , "  number of samples: %d"
                 ]
