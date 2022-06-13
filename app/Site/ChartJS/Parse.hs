{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.ChartJS.Parse (parseTableData, parseChart) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text as T (Text, unpack)
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
  chartType <- case lookup "type" kvs of
    Just "bar" -> Just Bar
    Just "line" -> Just Line
    Just "pie" -> Just Pie
    _ -> Nothing
  let scales = parseScales kvs
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
  indexAxis <- case fromMaybe "x" $ lookup "index-axis" kvs of
    "x" -> Just X
    "y" -> Just Y
    _ -> Nothing
  let legend = fromMaybe False $ lookupFlag "legend" kvs
  let plugins = [LegendPlugin legend, DataLabelsPlugin]
  options <- case chartType of
    Bar ->
      Just . OBar $
        BarOptions
          { barIndexAxis = indexAxis,
            barSkipNull = False,
            barScales = scales,
            barPlugins = plugins
          }
    _ -> Nothing
  pure $
    Chart
      { chartName = name,
        chartHeight = read . T.unpack <$> lookup "height" kvs,
        chartWidth = read . T.unpack <$> lookup "width" kvs,
        chartOptions = options,
        chartData =
          ChartData
            { dataLabels = mapMaybe (lookup labelKey) values,
              dataSets =
                [ DataSet
                    { dataSetValues = map (lookup valueKey) values,
                      dataSetLabel = fromMaybe valueKey $ lookup "label" kvs,
                      dataSetBorderColors = borderColors,
                      dataSetBackgroundColors = backgroundColors,
                      dataSetBorderWidth = 1,
                      dataSetFill = False
                    }
                ]
            }
      }

parseScales :: [(Text, Text)] -> Scales
parseScales kvs =
  Scales
    { scalesAxisX = Nothing,
      scalesAxisY =
        Just $
          AxisOptions
            { axisType = fromMaybe "linear" $ lookup "yAxisType" kvs,
              axisBeginAtZero = fromMaybe True $ lookupFlag "yAxisBeginAtZero" kvs
            }
    }

lookupFlag :: Text -> [(Text, Text)] -> Maybe Bool
lookupFlag key kvs = case lookup key kvs of
  Just "t" -> Just True
  Just "true" -> Just True
  Just "false" -> Just False
  _ -> Nothing
