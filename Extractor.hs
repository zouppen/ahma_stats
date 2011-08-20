module Extractor where

import Network.URI
import Text.JSON
import Database.CouchDB
import Data.DateTime
import Control.Monad (unless)

-- |Data structures for extraction from CouchDB.
data Query = Query { qDatabase :: URI
                   , qStart :: Maybe DateTime
                   , qEnd :: Maybe DateTime
                   } deriving (Show)

queryStats :: Query -> IO (Maybe JSValue)
queryStats q = runCouchDBURI (qDatabase q) (dbQuery q)

dbQuery :: Query -> CouchMonad (Maybe JSValue)
dbQuery q = do
  docs <- queryView (db "measurements") (doc "analysis") (doc "temp") range
  case length docs of 
    0 -> return Nothing
    1 -> return $ Just $ snd $ head docs
    _ -> fail "Too many results"

  where
    range = append "startkey" (qStart q) $ append "endkey" (qEnd q) []

append :: String -> Maybe DateTime -> [(String,JSValue)] -> [(String,JSValue)]
append key mbDate tail =
  case mbDate of
    Just date -> (key,showJSON $ toSeconds date):tail
    Nothing -> tail
