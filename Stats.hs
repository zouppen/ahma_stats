{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs.Implicit
import Network.URI
import Data.Maybe (fromJust)
import Data.DateTime
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Data.Time.LocalTime (TimeZone, localTimeToUTC, getCurrentTimeZone,
                            utcToLocalTime, timeZoneOffsetString)
import Text.Printf (printf)
import Extractor

data Args = Args { database :: String
                 , start :: Maybe String
                 , end :: Maybe String
                 , timezone :: String
                 } deriving (Show, Data, Typeable)

defDB = "http://localhost:5984"

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
  where tz = case zoneParse (timezone a) of
          Just x -> x
          Nothing -> error "Invalid timezone format. Use numeric definition!"

-- |Parses date and gives error in case of parse error. Absorbing
-- Maybe because you never want to continue in case of parse error.
parseWisely :: TimeZone -> String -> DateTime
parseWisely tz str =
  case parsed of
    Just a -> localTimeToUTC tz a
    Nothing -> error "Cannot parse date. ISO 8601 format required."
  where parsed = parseTime defaultTimeLocale "%F" str

-- |Parses numeric timezones.
zoneParse :: String -> Maybe TimeZone
zoneParse str = parseTime defaultTimeLocale "%z" str

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

-- |Formats time for given timezone.
showTime :: TimeZone -> DateTime -> String
showTime tz ts = show $ utcToLocalTime tz ts
