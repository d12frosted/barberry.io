{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Site.WinesTable (convert) where

import Data.Monoid (Any (..), getAny)
import Data.String (fromString)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Julius as J
import Text.Pandoc (Block (..), Cell (..), Pandoc (..), Row (..), TableHead (..))
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk

--------------------------------------------------------------------------------

convert :: Pandoc -> Pandoc
convert = walk go . embedImport
  where
    hasWinesTable = query $ \case
      Table (_, _, kvs) _ _ _ _ _ | ("class", "wines-table") `elem` kvs -> Any True
      _ -> mempty

    embedImport p@(Pandoc meta bs) =
      if getAny $ hasWinesTable p
        then
          Pandoc meta $
            importCSS :
            importJQuery :
            importBootstrapTable :
            importBootstrapTableTheme :
            sorters :
            bs
        else p

    go b = case b of
      Table (i, classes, kvs) caption colSpecs th tb tf
        | ("class", "wines-table") `elem` kvs ->
          Table (i, classes, kvs') caption colSpecs (goTH th) tb tf
        where
          kvs' =
            ("data-toggle", "table") :
            ("data-search", "true") :
            ("data-show-columns", "true") :
            ("data-show-columns-toggle-all", "true") :
            ("data-pagination", "false") :
            ("data-search-accent-neutralise", "true") :
            ("data-trim-on-search", "false") :
            ("data-search-highlight", "true") :
            kvs
      _ -> b

    goTH (TableHead attr rs) = TableHead attr (map goRow rs)
    goRow (Row attr cells) = Row attr (map goCell cells)
    goCell (Cell (i, c, kvs) alignment rowSpan colSpan bs) =
      Cell (i, c, kvs') alignment rowSpan colSpan bs
      where
        kvs' = [("data-sortable", "true"), ("data-field", field), ("data-sorter", sorter)] <> kvs
        field = stringify bs
        sorter = case field of
          "vintage" -> "vintageSorter"
          "rate" -> "ratingSorter"
          "amean" -> "ratingSorter"
          "rms" -> "ratingSorter"
          "QPR" -> "ratingSorter"
          "price" -> "ratingSorter"
          "producer" -> "htmlSorter"
          "name" -> "htmlSorter"
          _ -> "strSorter"

--------------------------------------------------------------------------------

importCSS :: Block
importCSS =
  htmlToBlock $
    H.link
      ! A.href "/library/bootstrap-table/dist/themes/bootstrap-table/bootstrap-table.min.css"
      ! A.rel "stylesheet"

importJQuery :: Block
importJQuery =
  htmlToBlock $
    H.script
      mempty
      ! A.src "/library/jquery/dist/jquery.min.js"

importBootstrapTable :: Block
importBootstrapTable =
  htmlToBlock $
    H.script mempty
      ! A.src "/library/bootstrap-table/dist/bootstrap-table.min.js"

importBootstrapTableTheme :: Block
importBootstrapTableTheme =
  htmlToBlock $
    H.script mempty
      ! A.src "/library/bootstrap-table/dist/themes/bootstrap-table/bootstrap-table.min.js"

--------------------------------------------------------------------------------

sorters :: Block
sorters =
  htmlToBlock . H.script . H.toHtml
    . J.renderJavascript
    $ [J.julius|
function ratingSorter(a, b) {
  const aa = a == '-' ? 0 : parseFloat(a);
  const bb = b == '-' ? 0 : parseFloat(b);
  return aa - bb;
}

function vintageSorter(a, b) {
  const aa = a == 'NV' ? 0 : parseFloat(a);
  const bb = b == 'NV' ? 0 : parseFloat(b);
  return aa - bb;
}

function htmlSorter(a, b) {
  return strSorter($(a).text(), $(b).text())
}

function strSorter(a, b) {
  return a.toLowerCase().localeCompare(b.toLowerCase());
}
      |]
      undefined

--------------------------------------------------------------------------------

htmlToBlock :: Html -> Block
htmlToBlock = RawBlock "html" . fromString . renderHtml
