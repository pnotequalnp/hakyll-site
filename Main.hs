module Main where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.String.Slugger (toSlug)
import Hakyll
import System.FilePath (takeExtension)
import Text.Pandoc.Highlighting (pygments, styleToCss)

main :: IO ()
main = hakyllWith defaultConfiguration do
  match ("templates/*" .||. "templates/**/*") do
    compile templateCompiler

  match ("robots.txt" .||. "image/*" .||. "image/**/*") do
    route idRoute
    compile copyFileCompiler

  match "CNAME" do
    route idRoute
    compile getResourceBody

  match ("index.md" .||. "about.md" .||. "404.md") do
    route (setExtension ".html")
    compile do
      pandocCompiler >>= loadAndApplyTemplate "templates/page.html" defaultContext

  match ("css/*" .||. "css/**/*") do
    route idRoute
    compile compressCssCompiler

  match posts do
    route postRoute
    compile do
      title <- getUnderlying >>= flip getMetadataField' "title"
      ext <- getUnderlyingExtension
      let sourcePath = toSlug title <> ext
          sourceContext = constField "source" sourcePath <> defaultContext
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= loadAndApplyTemplate "templates/source.html" sourceContext
        >>= loadAndApplyTemplate "templates/page.html" defaultContext

  match posts $ version "raw" do
    route sourceRoute
    compile getResourceBody

  match ("projects/minor/*" .||. "projects/major/*") do
    compile getResourceBody

  create ["posts.html"] do
    route idRoute
    compile do
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" postsContext
        >>= loadAndApplyTemplate "templates/page.html" defaultContext

  create ["projects.html"] do
    route idRoute
    compile do
      minor <- loadAll "projects/minor/*" >>= recentFirst
      major <- loadAll "projects/major/*" >>= recentFirst
      let projectsContext =
            constField "title" "Kevin Mullins - Projects"
              <> listField "minorProjects" defaultContext (pure minor)
              <> listField "majorProjects" defaultContext (pure major)
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/projects.html" projectsContext
        >>= loadAndApplyTemplate "templates/page.html" defaultContext

  create ["sitemap.xml"] do
    route idRoute
    compile do
      (init -> domain) <- loadBody "CNAME"
      let rootContext = constField "root" ("https://" <> domain)
          sitemapContext = rootContext <> postsContextWith (rootContext <> defaultContext) <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapContext

  create ["css/syntax/pygments.css"] do
    route idRoute
    compile do
      fmap compressCss <$> makeItem (styleToCss pygments)

posts :: Pattern
posts =
  ("posts/*.lhs" .||. "posts/*.md" .||. "posts/**/*.lhs" .||. "posts/**/*.md")
    .&&. complement "posts/posts.cabal"

postsContextWith :: Context String -> Context String
postsContextWith context =
  listField "posts" (slugContext <> context) $
    loadAll (posts .&&. hasNoVersion) >>= recentFirst
  where
    slugContext =
      Context \k _ (itemIdentifier -> identifier) -> do
        guard (k == "slug")
        StringField . toSlug <$> getMetadataField' identifier "title"

postsContext :: Context String
postsContext = postsContextWith defaultContext

postTitle :: Metadata -> String
postTitle = toSlug . fromMaybe (error "Post missing title") . lookupString "title"

postRoute :: Routes
postRoute = metadataRoute (constRoute . postTitle) `composeRoutes` setExtension ".html"

sourceRoute :: Routes
sourceRoute =
  metadataRoute $
    customRoute . \(postTitle -> title) (takeExtension . toFilePath -> ext) -> title <> ext
