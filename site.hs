{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative (..))
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (findIndex, intercalate, isPrefixOf, sortBy, tails)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Ord (Down (Down), comparing)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hakyll
import Path qualified as P

-- import System.FilePath (takeFileName)

postsGlob :: Pattern
postsGlob = "posts/**.md"

main :: IO ()
main = hakyll $ do
  match "assets/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  allPosts <- getMatches postsGlob
  let sortedPosts = sortIdentifiersByDate allPosts
      -- build hashmap of prev/next posts
      (nextPostHM, prevPostHM) = buildAdjacentPostsHashMap sortedPosts

  match "posts/*" $ do
    route $ foldl1 composeRoutes [gsubRoute "pages/" $ const "", setExtension "html", removeDatePrefix]
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
              <> defaultContext

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
    `mappend` defaultContext

removeDatePrefix :: Routes
removeDatePrefix = customRoute $ intercalate "-" . drop 3 . splitAll "-" . toFilePath

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
   in (fmap (maybe empty toUrl) . maybe empty getRoute) ident'

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
