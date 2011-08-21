module DbHelpers where

import Network.URI

defDB = "http://localhost:5984"

toURI s = case parseURI s of
  Just u -> u
  Nothing -> error "Syntax error in URL"
