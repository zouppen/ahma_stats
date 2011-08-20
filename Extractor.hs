module Extractor where

import Network.URI

-- |Data structures for extraction from CouchDB.
data Query = Query { qDatabase :: URI
                   , qStart :: Maybe Integer
                   , qEnd :: Maybe Integer
                   } deriving (Show)

-- TODO
queryStats :: a -> IO a
queryStats = return
