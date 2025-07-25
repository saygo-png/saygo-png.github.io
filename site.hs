{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative (..))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable, hashWithSalt)
import Data.List (findIndex, intercalate, isPrefixOf, sortBy, tails)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Ord (Down (Down), comparing)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Hakyll
import System.FilePath (takeFileName)

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
    route $ setExtension "html"
    compile $ do
      let postContextBut =
            field "nextPost" (lookupPostUrl nextPostHM)
              `mappend` postCtx
              `mappend` field "prevPost" (lookupPostUrl prevPostHM)

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
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx `mappend` bodyField "description"
      posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration feedCtx posts

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

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
      let filename = takeFileName $ toFilePath ident
          dateStr = intercalate "-" $ take 3 $ splitAll "-" filename
       in case parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr of
            Nothing -> error $ "Failed to parse date from filename: " ++ filename
            Just time -> time :: UTCTime
