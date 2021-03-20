-- Main module for the entry functions to the game
-- On purpose this file is kept small and compact
module Main where

import GameIO (startGame, config)

import Options.Applicative

main :: IO ()
main = execParser opts >>= startGame where
  opts = info ( config <**> helper )
    ( fullDesc 
    <> progDesc ("Play the Tic-Tac-Roll game, with an optional debugging file DEBUG.\n" ++ 
                "The computer by default waits and plays 2nd move.")
    <> header "PROG2006 -- Assignment 1 -- Tic-Tac-Roll")
