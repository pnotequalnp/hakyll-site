module Main where

import Data.Maybe (fromMaybe)
import Data.String.Slugger (toSlug)
import Hakyll
import System.FilePath (takeExtension)
import Text.Pandoc.Highlighting (pygments, styleToCss)

main :: IO ()
main = hakyllWith defaultConfiguration do
  match "templates/*" do
    compile templateCompiler

  match "image/*" do
    route idRoute
    compile copyFileCompiler

  match ("index.md" .||. "about.md" .||. "404.md") do
    route (setExtension ".html")
    compile do
      pandocCompiler >>= loadAndApplyTemplate "templates/page.html" defaultContext

  match "posts/*" do
    route postRoute
    compile do
      title <- getUnderlying >>= flip getMetadataField' "title"
      ext <- getUnderlyingExtension
      let sourcePath = toSlug title <> ext
          sourceContext = field "source" (const (pure sourcePath))
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/source.html" (sourceContext <> defaultContext)
        >>= loadAndApplyTemplate "templates/page.html" defaultContext

  match "posts/*" $ version "raw" do
    route sourceRoute
    compile getResourceBody

  match "css/*" do
    route idRoute
    compile compressCssCompiler

  create ["css/pygments.css"] do
    route idRoute
    compile do
      fmap compressCss <$> makeItem (styleToCss pygments)

postTitle :: Metadata -> String
postTitle = toSlug . fromMaybe (error "Post missing title") . lookupString "title"

postRoute :: Routes
postRoute = metadataRoute (constRoute . postTitle) `composeRoutes` setExtension ".html"

sourceRoute :: Routes
sourceRoute =
  metadataRoute $
    customRoute . \(postTitle -> title) (takeExtension . toFilePath -> ext) -> title <> ext
