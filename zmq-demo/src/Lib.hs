{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( publisher
    , subscriber
    ) where

import Control.Monad
import Data.String
import System.IO
import System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as CS


publisher :: IO ()
publisher = do
    let addr = "tcp://*:5555"
        name = "Mariusz: "
    runZMQ $ do
        pub <- socket Pub
        bind pub addr
        forever $ do
            line <- liftIO $ fromString <$> getLine
            send pub [] (name <> line)


subscriber :: IO ()
subscriber = do
    runZMQ $ do
        sub <- socket Sub
        subscribe sub ""
        connect sub "tcp://127.0.0.1:5555"
        forever $ do
            receive sub >>= liftIO . CS.putStrLn
            liftIO $ hFlush stdout

