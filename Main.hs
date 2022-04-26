module Main where

import Data.Foldable (traverse_)
import Hakyll

main :: IO ()
main = hakyllWith defaultConfiguration do
  match "templates/*" (compile templateCompiler)

  traverse_ serveStatic ["image/*"]
  where
    serveStatic r = match r do
      route idRoute
      compile copyFileCompiler
