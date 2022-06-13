{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad (filterM, (<=<))
import Control.Monad.Error.Class (liftEither)
import Data.List
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Monoid (Any (..), getAny)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll hiding (fromList)
import Site.ChartJS.Parse
import Site.ChartJS.Render
import Site.Web.Template.Context (modificationDateField)
import qualified Text.HTML.TagSoup as TS
import Text.Pandoc (Block (..), Inline (..), Pandoc (..))
import Text.Pandoc.Shared (mapLeft, stringify)
import Text.Pandoc.Walk

main :: IO ()
main = hakyll $ do
  now <- preprocess getCurrentTime

  match "node_modules/**" $ do
    route (gsubRoute "node_modules" (const "library"))
    compile copyFileCompiler

  match ("images/*" .||. "images/**/*") $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "assets/favicon/*" $ do
    route $ gsubRoute "assets/favicon/" (const "")
    compile copyFileCompiler

  match "assets/fonts/*" $ do
    route $ gsubRoute "assets/" (const "")
    compile copyFileCompiler

  match "pages/intro.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile customPandocCompiler

  match "pages/reviews-latest.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile customPandocCompiler

  match "pages/reviews.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile customPandocCompiler

  match postsPattern $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= adaptTables
        >>= relativizeUrls

  create ["posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllPosts now
      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Archives"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "wines/*.org" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/wine.html" wineCtx
        >>= loadAndApplyTemplate "templates/default.html" wineCtx
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
        >>= relativizeUrls

  match "pages/index.html" $ do
    route $ gsubRoute "pages/" (const "")
    compile $ do
      posts <- recentFirst =<< loadAllPosts now
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
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> modificationDateField "update" "%B %e, %Y"
    <> defaultContext

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

adaptTables :: Item String -> Compiler (Item String)
adaptTables = pure . fmap (withTagList f)
  where
    f ((TS.TagOpen "table" as) : ts) =
      [TS.TagOpen "div" divAttrs, TS.TagOpen "table" (as <> tblAttrs)] <> f ts
    f ((TS.TagClose "table") : ts) =
      [TS.TagClose "table", TS.TagClose "div"] <> f ts
    f ((TS.TagOpen "td" tdAttrs) : (TS.TagOpen "strong" _) : ts) =
      [TS.TagOpen "td" (tdAttrs <> [("class", "highlight-successful")])] <> f ts
    f ((TS.TagClose "strong") : (TS.TagClose "td") : ts) =
      [TS.TagClose "td"] <> f ts
    f ((TS.TagOpen "td" tdAttrs) : (TS.TagOpen "del" _) : ts) =
      [TS.TagOpen "td" (tdAttrs <> [("class", "highlight-critical")])] <> f ts
    f ((TS.TagClose "del") : (TS.TagClose "td") : ts) =
      [TS.TagClose "td"] <> f ts
    f (t : ts) = t : f ts
    f [] = []

    tblAttrs =
      [ ("rules", "groups"),
        ("cellspacing", "0"),
        ("cellpadding", "6")
      ]
    divAttrs = [("class", "table-container")]

--------------------------------------------------------------------------------

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransformM readerOptions writerOptions transform
  where
    readerOptions = defaultHakyllReaderOptions
    writerOptions = defaultHakyllWriterOptions
    transform = convertBarberryLinks <=< embedChartJS . standartizeStars

embedChartJS :: Pandoc -> Compiler Pandoc
embedChartJS pandoc = walkM embedChart . embedImport $ pandoc
  where
    queryTable :: Text -> Maybe Block
    queryTable name = listToMaybe . flip query pandoc $ \case
      t@(Table (n, _, _) _ _ _ _ _) | n == name -> [t]
      _ -> []

    hasChartJS = query $ \case
      Div (_, cs, _) _ | "chartjs" `elem` cs -> Any True
      _ -> mempty

    embedImport p@(Pandoc meta bs) =
      if getAny $ hasChartJS p
        then Pandoc meta (RawBlock "html" importStatement : bs)
        else p

    embedChart = liftEither . mapLeft (\e -> [T.unpack e]) . go []
    go names (b : bs) = case b of
      Div (name, _, _) _ | name `elem` names -> go names bs
      Table (name, _, _) _ _ _ _ _ | name `elem` names -> go names bs
      Div (name, cs, kvs) _ | "chartjs" `elem` cs -> do
        dataName <- noteMaybe name "missing 'data'" $ lookup "data" kvs
        dataBlock <- noteMaybe name ("missing table named " <> dataName) $ queryTable dataName
        tableData <- parseTableData name dataBlock
        chart <- parseChart name kvs tableData
        let chartHtml = RawBlock "html" $ render chart
        (chartHtml :) <$> go (dataName : names) bs
      _ -> (b :) <$> go names bs
    go _ [] = pure []

standartizeStars :: Pandoc -> Pandoc
standartizeStars = walk $ \case
  Str "\9733" -> star
  Str "\9734" -> star
  i -> i
  where
    star = Span ("", ["material-symbols-outlined"], []) [RawInline "html" "&#xE838;"]

convertBarberryLinks :: Pandoc -> Compiler Pandoc
convertBarberryLinks = walkM $ \case
  link@(Link a is (url, title)) -> case T.stripPrefix "barberry:" url of
    Nothing -> pure link
    Just url' -> do
      let i = fromFilePath . T.unpack . (<> ".org") . fromMaybe url' . T.stripPrefix "/" $ url'
      exists <- isJust <$> getRoute i
      pure $ if exists then Link a is (url' <> ".html", title) else Str (stringify is)
  i -> pure i

--------------------------------------------------------------------------------

postsPattern :: Pattern
postsPattern = "posts/*.org"

loadAllPosts :: UTCTime -> Compiler [Item String]
loadAllPosts = loadPosts postsPattern

loadPosts :: Pattern -> UTCTime -> Compiler [Item String]
loadPosts pat now =
  skipDrafts
    =<< skipAfter now
    =<< recentFirst
    =<< loadAll pat

skipAfter :: (MonadFail m, MonadMetadata m) => UTCTime -> [Item a] -> m [Item a]
skipAfter now =
  filterM $
    fmap (now >)
      . getItemUTC defaultTimeLocale
      . itemIdentifier

skipDrafts :: (MonadMetadata m) => [Item a] -> m [Item a]
skipDrafts = filterM publish
  where
    publish i =
      maybe True asFlag
        <$> getMetadataField (itemIdentifier i) "publish"
    asFlag "true" = True
    asFlag _ = False

--------------------------------------------------------------------------------
