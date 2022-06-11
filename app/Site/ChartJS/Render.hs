{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site.ChartJS.Render (renderToText) where

import Data.Aeson as Aeson
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Site.ChartJS.Types
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Text.Julius as J

--------------------------------------------------------------------------------

renderToText :: (ToJSON a, IsString b) => Chart a -> b
renderToText = fromString . renderHtml . renderToHtml

--------------------------------------------------------------------------------

renderToHtml :: ToJSON a => Chart a -> Html
renderToHtml chart@(Chart {..}) = aDiv $ canvas <> script
  where
    canvas =
      H.canvas mempty
        ! A.id (fromText cName)
        ! maybe mempty (height . fromText) cHeight
        ! maybe mempty (width . fromText) cWidth
    script = H.script . H.toHtml . chartJs $ chart
    aDiv = H.div ! A.class_ "chartjs"

--------------------------------------------------------------------------------

chartJs :: ToJSON a => Chart a -> LT.Text
chartJs Chart {..} =
  let labels = toJSON $ cdLabels cData
      dataSets = toJSON $ cdDataSets cData
   in J.renderJavascript $
        [J.julius|
      new Chart(document.getElementById(#{cName}).getContext('2d'), {
        plugins: [ChartDataLabels],
        type: #{cType},
        data: {
          labels: #{labels},
          datasets: #{dataSets}
        },
        options: {
          // maintainAspectRatio: false,
          title: {
            display: #{isJust cTitle},
            text: #{fromMaybe "" cTitle}
          },
          scales: #{toJSON cScales},
          plugins: {
            legend: {
              display: #{cDisplayLegend}
            },
            datalabels: {
              anchor: 'end',
              align: 'start'
            }
          }
        }
      });
      |]
          undefined

--------------------------------------------------------------------------------

fromText :: IsString a => Text -> a
fromText = fromString . T.unpack
