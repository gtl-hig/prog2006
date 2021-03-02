{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getNextJoke
    ) where


import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple


-- Constant for the API
chuckAPI :: Request
chuckAPI = "http://api.chucknorris.io/jokes/random"

--  Joke is the struct used to  unmarshal the JSON response from the URL
data Joke = Joke {
                   categories:: [String]
                 , icon_url  :: String
                 , id        :: String
                 , url       :: String
                 , value     :: String
                 } deriving (Show, Generic)

instance FromJSON Joke
instance ToJSON Joke

-- | Main HTTP API fetching logic.
-- http-conduit offers a high-level API that does JSON parsing "by default"
-- and returns @IO <type>@ that has been parsed. We fetch the response,
-- parse it, extract value (the content of the joke) and return it as IO String.
getNextJoke :: IO String
getNextJoke = value . getResponseBody <$> httpJSON chuckAPI
{--
Same code as above, but written with do-notation

    do
        response <- httpJSON chuckAPI
        return . value . getResponseBody $ response
--}