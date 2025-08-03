{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Applicative (Alternative (..))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (intercalate, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down), comparing)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hakyll
import Path qualified as P

postsGlob :: Pattern
postsGlob = "posts/**.md"

main :: IO ()
main = hakyll $ do
  match "assets/*" $ do
    route idRoute
    compile copyFileCompiler

  -- No route because we inline it with a context
  -- We just need to make it available.
  match "css/*" $ compile getResourceBody

  allPosts <- getMatches postsGlob
  let sortedPosts = sortIdentifiersByDate allPosts
      -- build hashmap of prev/next posts
      (nextPostHM, prevPostHM) = buildAdjacentPostsHashMap sortedPosts

  match "posts/*" $ do
    route $
      foldl1
        composeRoutes
        [ gsubRoute "pages/" $ const ""
        , removeDatePrefix
        , setExtension ""
        , dirWithIndexHtml
        ]
    compile $ do
      let postContextBut =
            field "nextPost" (lookupPostUrl nextPostHM)
              <> postCtx
              <> field "prevPost" (lookupPostUrl prevPostHM)

      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postContextBut
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" postContextBut
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> siteContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration feedCtx posts

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` siteContext

siteContext :: Context String
siteContext =
  field "url" clean
    <> inlineCssContext
    <> defaultContext
  where
    clean item = do
      path <- getRoute (itemIdentifier item)
      case path of
        Nothing -> noResult "no route for identifier"
        Just s -> pure . removeIndexSuffix . toUrl $ s

    inlineCssContext = field "inlineCss" $ \_ ->
      compressCss <$> loadBody (fromFilePath "css/default.css")

removeDatePrefix :: Routes
removeDatePrefix = customRoute $ intercalate "-" . drop 3 . splitAll "-" . toFilePath

-- https://chungyc.org/article/technical/website/extensionless
removeIndexSuffix :: String -> String
removeIndexSuffix url@('/' : _) -- only clean up local URLs
  | Nothing <- prefix = url -- does not end with index.html
  | Just s <- prefix = s -- clean up index.html from URL
  where
    prefix = needlePrefix "index.html" url
removeIndexSuffix url = url

dirWithIndexHtml :: Routes
dirWithIndexHtml = customRoute $ \i ->
  let path = toFilePath i in path <> "/" <> "index.html"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
  FeedConfiguration
    { feedTitle = "Saygo's blog"
    , feedDescription = "This feed provides my blog posts"
    , feedAuthorName = "Saygo"
    , feedAuthorEmail = "saygo.mail@proton.me"
    , feedRoot = "https://saygo-png.github.io"
    }

type AdjPostHM = HM.HashMap Identifier Identifier

instance Hashable Identifier where
  hashWithSalt salt = hashWithSalt salt . show

buildAdjacentPostsHashMap :: [Identifier] -> (AdjPostHM, AdjPostHM)
buildAdjacentPostsHashMap posts =
  let buildHM :: [Identifier] -> [Identifier] -> AdjPostHM
      buildHM [] _ = HM.empty
      buildHM _ [] = HM.empty
      buildHM (k : ks) (v : vs) = HM.insert k v $ buildHM ks vs
   in (buildHM (drop 1 posts) posts, buildHM posts (drop 1 posts))

lookupPostUrl :: AdjPostHM -> Item String -> Compiler String
lookupPostUrl hm post =
  let ident = itemIdentifier post
      ident' = HM.lookup ident hm
   in (fmap (maybe empty (removeIndexSuffix . toUrl)) . maybe empty getRoute) ident'

sortIdentifiersByDate :: [Identifier] -> [Identifier]
sortIdentifiersByDate = sortBy (comparing (Down . parseDate))
  where
    parseDate ident =
      let path = fromMaybe (error "Invalid path") $ P.parseRelFile $ toFilePath ident
          filename = P.filename path
          dateStr = intercalate "-" . take 3 $ splitAll "-" $ P.fromRelFile filename
       in case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
            Nothing -> error $ "Failed to parse date from path: " ++ P.fromRelFile path
            Just time -> time :: UTCTime
