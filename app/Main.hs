module Main where

import SquareLimit
import System.Environment

main :: IO ()
main =
  withArgs ["-o", "sqlimit.svg", "-w", "400"] mainMethod
