{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.ChartJS.Parse (parseTableData, parseChart) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Site.ChartJS.Types
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk

--------------------------------------------------------------------------------

parseTableData :: Block -> Maybe TableData
parseTableData (Table _ _ _ th [tb] _) = Just $ TableData aHeader (toValues tb)
  where
    aHeader = toHeader th
    toHeader = queryTableHead $ \case
      Plain ins -> [stringify ins]
      _ -> []
    toValues (TableBody _ _ _ rows) = zip aHeader . fmap stringify . getCells <$> rows
    getCells (Row _ cells) = cells
parseTableData _ = Nothing

parseChart :: Text -> [(Text, Text)] -> TableData -> Maybe (Chart Text)
parseChart name kvs (TableData _ values) = do
  labelKey <- lookup "labels" kvs
  valueKey <- lookup "values" kvs
  let chartType = fromMaybe "line" $ lookup "type" kvs
  let scales = parseScales chartType kvs
  let backgroundColors =
        [ "rgba(255, 99, 132, 0.2)",
          "rgba(54, 162, 235, 0.2)",
          "rgba(255, 206, 86, 0.2)",
          "rgba(75, 192, 192, 0.2)",
          "rgba(153, 102, 255, 0.2)",
          "rgba(255, 159, 64, 0.2)"
        ]
  let borderColors =
        [ "rgba(255, 99, 132, 1)",
          "rgba(54, 162, 235, 1)",
          "rgba(255, 206, 86, 1)",
          "rgba(75, 192, 192, 1)",
          "rgba(153, 102, 255, 1)",
          "rgba(255, 159, 64, 1)"
        ]
  pure $
    Chart
      { cName = name,
        cTitle = lookup "title" kvs,
        cHeight = lookup "height" kvs,
        cWidth = lookup "width" kvs,
        cType = chartType,
        cDisplayLegend = fromMaybe False $ lookupFlag "legend" kvs,
        cScales = scales,
        cData =
          ChartData
            { cdLabels = mapMaybe (lookup labelKey) values,
              cdDataSets =
                [ ChartDataSet
                    { cdsData = map (lookup valueKey) values,
                      cdsLabel = fromMaybe valueKey $ lookup "label" kvs,
                      cdsBorderColors = borderColors,
                      cdsBackgroundColors = backgroundColors,
                      cdsBorderWidth = 1,
                      cdsFill = False
                    }
                ]
            }
      }

parseScales :: Text -> [(Text, Text)] -> ChartScales
parseScales "horizontalBar" kvs =
  ChartScales
    { csAxisX =
        Just $
          ChartAxis
            { caType = fromMaybe "linear" $ lookup "xAxisType" kvs,
              caBeginAtZero = fromMaybe True $ lookupFlag "xAxisBeginAtZero" kvs
            },
      csAxisY = Nothing
    }
parseScales _ kvs =
  ChartScales
    { csAxisX = Nothing,
      csAxisY =
        Just $
          ChartAxis
            { caType = fromMaybe "linear" $ lookup "yAxisType" kvs,
              caBeginAtZero = fromMaybe True $ lookupFlag "yAxisBeginAtZero" kvs
            }
    }

lookupFlag :: Text -> [(Text, Text)] -> Maybe Bool
lookupFlag key kvs = case lookup key kvs of
  Just "t" -> Just True
  Just "true" -> Just True
  Just "false" -> Just False
  _ -> Nothing
