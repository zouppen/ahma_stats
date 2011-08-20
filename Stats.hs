{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs.Implicit
import Network.URI
import Data.Maybe (fromJust)
import Data.DateTime
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Data.Time.LocalTime (TimeZone, localTimeToUTC, getCurrentTimeZone,
                            utcToLocalTime)
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
      , timezone = show tz &= typ "TZ" &= help 
                   ("Timezone (default: " ++ show tz ++ ")")
      }
  &= program "stats"
  &= summary "Ahma stats 0.1"
  &= help "Extracts measurements from a database."

convertArgs :: Args -> Query
convertArgs a = Query { qDatabase = fromJust $ parseURI (database a)
                      , qStart = start a >>= Just . (parseWisely tz)
                      , qEnd = end a >>= Just . (parseWisely tz)
                      }
  where tz = read $ timezone a

-- |Parses date and gives error in case of parse error. Absorbing
-- Maybe because you never want to continue in case of parse error.
parseWisely :: TimeZone -> String -> DateTime
parseWisely tz str =
  case parsed of
    Just a -> localTimeToUTC tz a
    Nothing -> error "Cannot parse date. ISO 8601 format required."
  where parsed = parseTime defaultTimeLocale "%F" str

main = do 
  tz <- getCurrentTimeZone
  args <- cmdArgs (synopsis tz)
  stats <- queryStats $ convertArgs args
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

showTime :: TimeZone -> DateTime -> String
showTime tz ts = show $ utcToLocalTime tz ts
