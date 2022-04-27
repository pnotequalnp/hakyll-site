module Main where

import Hakyll

main :: IO ()
main = hakyllWith defaultConfiguration do
  match "templates/*" do
    compile templateCompiler

  match staticFiles do
    route idRoute
    compile copyFileCompiler

  match "404.md" do
    route (setExtension ".html")
    compile (pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext)

  match "posts/*" do
    route (setExtension ".html")
    compile (pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext)

staticFiles :: Pattern
staticFiles = "image/*"
