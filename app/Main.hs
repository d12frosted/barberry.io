{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List
import Data.Ord
import Hakyll

main :: IO ()
main = hakyll $ do
  match "images/**/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "assets/favicon/*" $ do
    route $ gsubRoute "assets/favicon/" (const "")
    compile copyFileCompiler

  match "pages/intro.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile pandocCompiler

  match "pages/reviews-latest.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile pandocCompiler

  match "pages/reviews.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile pandocCompiler

  -- match (fromList ["pages/about.org", "pages/contact.markdown"]) $ do
  --   route $ gsubRoute "pages/" (const "") <> setExtension "html"
  --   compile $
  --     pandocCompiler
  --       >>= loadAndApplyTemplate "templates/default.html" defaultContext
  --       >>= relativizeUrls

  match "posts/*.org" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= fixUrls
        >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.org"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= fixUrls
        >>= relativizeUrls

  match "wines/*.org" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/wine.html" wineCtx
        >>= loadAndApplyTemplate "templates/default.html" wineCtx
        >>= fixUrls
        >>= relativizeUrls

  create ["wines.html"] $ do
    route idRoute
    compile $ do
      wines <- titleOrdered =<< loadAll "wines/*.org"
      let archiveCtx =
            listField "wines" wineCtx (return wines)
              `mappend` constField "title" "Wines"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/wines.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= fixUrls
        >>= relativizeUrls

  create ["reviews.html"] $ do
    route idRoute
    compile $ do
      reviews <- load "pages/reviews.org"
      let ctx =
            field "reviews" (const . return . itemBody $ reviews)
              `mappend` constField "title" "Reviews"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/reviews.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= fixUrls
        >>= relativizeUrls

  match "pages/index.html" $ do
    route $ gsubRoute "pages/" (const "")
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*.org"
      intro <- load "pages/intro.org"
      reviews <- load "pages/reviews-latest.org"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> field "intro" (const . return . itemBody $ intro)
              <> field "latest-reviews" (const . return . itemBody $ reviews)
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= fixUrls
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

wineCtx :: Context String
wineCtx = defaultContext

--------------------------------------------------------------------------------

titleOrdered :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
titleOrdered = sortByM $ getTitle . itemIdentifier
  where
    getTitle :: (MonadMetadata m, MonadFail m) => Identifier -> m String
    getTitle i = do
      producer <- getMetadataField' i "producer"
      name <- getMetadataField' i "name"
      vintage <- getMetadataField' i "vintage"
      return $ producer <> " " <> name <> " " <> vintage

    sortByM :: (Monad m, Ord k) => (a -> m k) -> [a] -> m [a]
    sortByM f xs =
      map fst . sortBy (comparing snd)
        <$> mapM (\x -> fmap (x,) (f x)) xs

--------------------------------------------------------------------------------

fixUrls :: Item String -> Compiler (Item String)
fixUrls = pure . fmap (withUrls fix)
  where
    fix x = maybe x wrap (stripPrefix "barberry:" x)
    wrap x = x <> ".html"

--------------------------------------------------------------------------------
