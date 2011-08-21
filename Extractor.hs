module Extractor where

import Control.Monad (unless)
import Data.DateTime
import Database.CouchDB
import Network.URI
import Text.JSON

-- |Incoming data which defines the query.
data QueryStats = QueryStats { qsDatabase :: URI
                             , qsStart    :: Maybe DateTime
                             , qsEnd      :: Maybe DateTime
                             } deriving (Show)

-- |Incoming data which defines single document query.
data QueryOne = QueryOne { qoDatabase :: URI
                         , qoWhen     :: Maybe DateTime
                         } deriving (Show)

-- |Outgoing data contains one result.
data TempStat = TempStat { minTemp :: Double
                         , maxTemp :: Double
                         , avg     :: Double 
                         , minTime :: DateTime
                         , maxTime :: DateTime
                         , n       :: Integer
                         } deriving (Show)

queryStats q = runCouchDBURI (qsDatabase q) (dbQueryStats q)

dbQueryStats :: QueryStats -> CouchMonad (Maybe [(String, TempStat)])
dbQueryStats q = do
  docs <- queryTempView range
  case length docs of
    0 -> return Nothing -- Date range too narrow, no data.
    1 -> return $ Just $ map conv $ fromJSObject $ snd $ head docs
    _ -> fail "Too many results"
  where
    range = append "startkey" (qsStart q) $ append "endkey" (qsEnd q) []
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

queryOne q = runCouchDBURI (qoDatabase q) (dbQueryOne q)

dbQueryOne :: QueryOne -> CouchMonad (Maybe (JSObject JSValue))
dbQueryOne q = do
  docs <- queryTempView range
  case docs of
    [(doc,_)] -> do
      Just (_,_,a) <- getDoc (db "measurements") doc
      return $ Just a
    [] -> return Nothing -- Date too old or no data.
    _ -> fail "Database is bad."
  where
    range = append "startkey" (qoWhen q) static
    static = [("descending",JSBool True)
             ,("reduce",JSBool False)
             ,("limit",showJSON (1::Integer))]

queryTempView = queryView (db "measurements") (doc "analysis") (doc "temp")