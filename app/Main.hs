{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (filterM, (<=<))
import Control.Monad.Error.Class (liftEither)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Monoid (Any (..), getAny)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Hakyll hiding (fromList)
import Site.ChartJS.Parse
import Site.ChartJS.Render
import Site.Web.Template.Context (modificationDateField)
import qualified Site.WinesTable as WinesTable
import System.FilePath (dropExtension, takeExtension)
import Text.Pandoc (Block (..), Cell (..), Inline (..), Pandoc (..), Row (..), TableBody (..))
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

  match "pages/wines.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile customPandocCompiler

  match "pages/cellar.org" $ do
    route $ gsubRoute "pages/" (const "") <> setExtension "html"
    compile customPandocCompiler

  match "posts/*.org" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
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

  match "convives/*.org" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/convive.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "producers/*.org" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/producer.html" producerCtx
        >>= loadAndApplyTemplate "templates/default.html" producerCtx
        >>= relativizeUrls

  match "wines/*.org" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/wine.html" wineCtx
        >>= loadAndApplyTemplate "templates/default.html" wineCtx
        >>= relativizeUrls

  match "wines/*.json" $ do
    route idRoute
    compile copyFileCompiler

  create ["wines.html"] $ do
    route idRoute
    compile $ do
      wines <- load "pages/wines.org"
      let ctx = field "wines" (const . return . itemBody $ wines) <> constField "title" "Wines" <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/wines.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  create ["reviews.html"] $ do
    route idRoute
    compile $ do
      reviews <- load "pages/reviews.org"
      let ctx =
            field "reviews" (const . return . itemBody $ reviews)
              <> constField "title" "Reviews"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/reviews.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  create ["cellar.html"] $ do
    route idRoute
    compile $ do
      reviews <- load "pages/cellar.org"
      let ctx =
            field "reviews" (const . return . itemBody $ reviews)
              <> constField "title" "Cellar"
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/reviews.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "pages/subscribe.html" $ do
    route $ gsubRoute "pages/" (const "")
    compile $ do
      let subscribeCtx = defaultContext
      getResourceBody
        >>= applyAsTemplate subscribeCtx
        >>= loadAndApplyTemplate "templates/default.html" subscribeCtx
        >>= relativizeUrls

  match "pages/ml-subscribe.html" $ do
    route $ gsubRoute "pages/" (const "")
    compile $ do
      getResourceBody >>= relativizeUrls

  match "pages/index.html" $ do
    route $ gsubRoute "pages/" (const "")
    compile $ do
      posts <- recentFirst =<< loadAllPosts now
      intro <- load "pages/intro.org"
      reviews <- load "pages/reviews-latest.org"
      let indexCtx =
            listField "posts" postCtx (return $ take 8 posts)
              <> field "intro" (const . return . itemBody $ intro)
              <> field "latest-reviews" (const . return . itemBody $ reviews)
              <> constField "index-page" "true"
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
    <> dateField "date-iso" "%F"
    <> modificationDateField "update-iso" "%F"
    <> defaultContext

wineCtx :: Context String
wineCtx = defaultContext

producerCtx :: Context String
producerCtx =
  dateField "date" "%B %e, %Y"
    <> modificationDateField "update" "%B %e, %Y"
    <> dateField "date-iso" "%F"
    <> modificationDateField "update-iso" "%F"
    <> defaultContext

--------------------------------------------------------------------------------

pandoncTrasnformM :: Pandoc -> Compiler Pandoc
pandoncTrasnformM =
  convertBarberryLinks
    <=< (pure . wrapTables . processBottleImages . processTastingScores . WinesTable.convert)
    <=< embedChartJS . standartizeStars

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransformM readerOptions writerOptions transform
  where
    readerOptions = defaultHakyllReaderOptions
    writerOptions = defaultHakyllWriterOptions
    transform = pandoncTrasnformM

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
        chart <- parseChart name cs kvs tableData
        let chartHtml = RawBlock "html" $ render chart
        (chartHtml :) <$> go (dataName : names) bs
      _ -> (b :) <$> go names bs
    go _ [] = pure []

standartizeStars :: Pandoc -> Pandoc
standartizeStars = walk $ \case
  Str "\9733" -> symbol "&#xE838;"
  Str "\9734" -> symbol "&#xE838;"
  Str "\128148" -> symbol "&#xEAC2;"
  Str "\10084\65039" -> symbol "&#xE87D;"
  Str "\10084" -> symbol "&#xE87D;"
  Str "&nbsp;" -> symbol "&nbsp;"
  i -> i
  where
    symbol x = Span ("", ["material-symbols-outlined"], []) [RawInline "html" x]

convertBarberryLinks :: Pandoc -> Compiler Pandoc
convertBarberryLinks = walkM $ \case
  link@(Link a is (urlRaw, title)) -> case T.stripPrefix "barberry:" urlRaw of
    Nothing -> pure link
    Just url' -> do
      let url = fromMaybe url' $ T.stripSuffix ".html" url'
      let i = fromFilePath . T.unpack . (<> ".org") . fromMaybe url . T.stripPrefix "/" $ url
      exists <- isJust <$> getRoute i
      pure $ if exists then Link a is (url <> ".html", title) else Str (stringify is)
  i -> pure i

wrapTables :: Pandoc -> Pandoc
wrapTables = walk go
  where
    go [] = []
    go (b : bs) = case b of
      Table {} -> Div ("", ["table-container"], []) [b] : go bs
      _ -> b : go bs

processTastingScores :: Pandoc -> Pandoc
processTastingScores = walk $ \case
  Table (i, classes, kvs) caption colSpecs th tb tf
    | ("class", "tasting-scores") `elem` kvs ->
      Table (i, classes, kvs) caption colSpecs th (map goTB tb) tf
    where
      goTB (TableBody tba tbh rs1 rs2) = TableBody tba tbh (map goRow rs1) (map goRow rs2)
      goRow (Row ra cells) = Row ra $ map goCell cells
      goCell (Cell (ci, cc, ckvs) alignment rowSpan colSpan bs) =
        Cell (ci, cc', ckvs) alignment rowSpan colSpan (clean bs)
        where
          cc' =
            ["highlight-successful" | getAny $ hasStrong bs]
              <> ["highlight-critical" | getAny $ hasStrikeout bs]
              <> cc
          clean = walk $ \case
            Strong is -> Str . stringify $ is
            Strikeout is -> Str . stringify $ is
            inline -> inline
          hasStrong = query $ \case
            Strong _ -> Any True
            _ -> mempty
          hasStrikeout = query $ \case
            Strikeout _ -> Any True
            _ -> mempty
  b -> b

processBottleImages :: Pandoc -> Pandoc
processBottleImages = walk $ \case
  Image a@(_, _, kvs) is (urlRaw, title)
    | ("class", "bottle-right") `elem` kvs ->
      Image a is (urlRaw', title)
    where
      urlRawStr = T.unpack urlRaw
      urlRaw' = T.pack $ dropExtension urlRawStr <> "@512" <> takeExtension urlRawStr
  i -> i

--------------------------------------------------------------------------------

loadAllPosts :: UTCTime -> Compiler [Item String]
loadAllPosts = loadPosts

loadPosts :: UTCTime -> Compiler [Item String]
loadPosts now =
  skipDrafts
    =<< skipAfter now
    =<< recentFirst
    =<< keepPosts
    =<< loadAll ("posts/*.org" .||. "producers/*.org")

skipAfter :: (MonadFail m, MonadMetadata m) => UTCTime -> [Item a] -> m [Item a]
skipAfter now =
  filterM $
    fmap (now >)
      . getItemUTC defaultTimeLocale
      . itemIdentifier

skipDrafts :: (MonadMetadata m) => [Item a] -> m [Item a]
skipDrafts = filterM hide
  where
    hide i =
      maybe False (not . asFlag)
        <$> getMetadataField (itemIdentifier i) "hide"

keepPosts :: (MonadMetadata m) => [Item a] -> m [Item a]
keepPosts = filterM publish
  where
    publish i =
      maybe True asFlag
        <$> getMetadataField (itemIdentifier i) "publish"

asFlag :: String -> Bool
asFlag "true" = True
asFlag _ = False

--------------------------------------------------------------------------------
