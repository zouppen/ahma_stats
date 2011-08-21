-- |Tries to make using of the time library included in Haskell
-- Platform a bit less frustrating.
module TimeQuirks where

import Data.DateTime
import Data.Time.Format (parseTime)
import Data.Time.LocalTime (TimeZone, localTimeToUTC, utcToLocalTime)
import System.Locale (defaultTimeLocale)

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

-- |Like zoneParse, but returns an error if time zone is invalid.
zoneParse' :: String -> TimeZone
zoneParse' str = case zoneParse str of
  Just x -> x
  Nothing -> error "Invalid timezone format. Use numeric definition!"

-- |Formats time for given timezone.
showTime :: TimeZone -> DateTime -> String
showTime tz ts = show $ utcToLocalTime tz ts
