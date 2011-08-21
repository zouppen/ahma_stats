{-*- coding: utf-8 -*-}
module Extractor where

import Network.URI
import Text.JSON
import Database.CouchDB
import Data.DateTime
import Control.Monad (unless)

-- |Incoming data which defines the query.
data Query = Query { qDatabase :: URI
                   , qStart :: Maybe DateTime
                   , qEnd :: Maybe DateTime
                   } deriving (Show)

-- |Outgoing data contains one result.
data TempStat = TempStat { minTemp :: Double
                         , maxTemp :: Double
                         , avg     :: Double 
                         , minTime :: DateTime
                         , maxTime :: DateTime
                         , n       :: Integer
                         } deriving (Show)

--queryStats :: Query -> IO (Maybe [TempStat])
queryStats q = runCouchDBURI (qDatabase q) (dbQuery q)

dbQuery :: Query -> CouchMonad (Maybe [(String, TempStat)])
dbQuery q = do
  docs <- queryView (db "measurements") (doc "analysis") (doc "temp") range
  case length docs of
    0 -> return Nothing -- Date range too narrow, no data.
    1 -> return $ Just $ map conv $ fromJSObject $ snd $ head docs
    _ -> fail "Too many results"

  where
    range = append "startkey" (qStart q) $ append "endkey" (qEnd q) []
    conv (a,b) = (a,extract b)

append :: String -> Maybe DateTime -> [(String,JSValue)] -> [(String,JSValue)]
append key mbDate tail =
  case mbDate of
    Just date -> (key,showJSON $ toSeconds date):tail
    Nothing -> tail

-- |Extracts TempStat from JSON. Does the type conversions
-- brutally. If database format doesn't match, an error is returned.
extract :: JSObject JSValue -> TempStat
extract js = TempStat { minTemp = readQuick "min"
                      , maxTemp = readQuick "max"
                      , avg = avg_
                      , minTime = fromSeconds $ readQuick "min_time"
                      , maxTime = fromSeconds $ readQuick "max_time"
                      , n = n_
                      }
  where
    avg_ = readQuick "sum" / fromIntegral n_
    n_ = readQuick "n"
    readQuick x = case valFromObj x js of
      Ok a -> a
      Error msg -> error $ "Database format error: " ++ msg
