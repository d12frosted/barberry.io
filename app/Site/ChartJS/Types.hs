{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Site.ChartJS.Types where

import Data.Aeson as Aeson hiding (Options)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

--------------------------------------------------------------------------------

data TableData = TableData [Text] [[(Text, Text)]]

--------------------------------------------------------------------------------

data ChartType = Bar | Line | Pie | Doughnut deriving (Show)

instance ToJSON ChartType where
  toJSON Bar = "bar"
  toJSON Line = "line"
  toJSON Pie = "pie"
  toJSON Doughnut = "doughnut"

--------------------------------------------------------------------------------

data SizeMode = StaticSize | DynamicSize deriving (Show, Eq)

data Chart a = Chart
  { chartName :: Text,
    chartSizeMode :: SizeMode,
    chartOptions :: Options,
    chartData :: ChartData a,
    chartClass :: [Text]
  }
  deriving (Show)

--------------------------------------------------------------------------------

data Axis = X | Y deriving (Show)

instance ToJSON Axis where
  toJSON X = "x"
  toJSON Y = "y"

--------------------------------------------------------------------------------

data Options
  = OBar BarOptions
  | OLine LineOptions
  | OPie PieOptions
  | ODoughnut DoughnutOptions
  deriving (Show)

instance ToJSON Options where
  toJSON (OBar o) = toJSON o
  toJSON (OLine o) = toJSON o
  toJSON (OPie o) = toJSON o
  toJSON (ODoughnut o) = toJSON o

--------------------------------------------------------------------------------

data Anchor
  = AnchorStart
  | AnchorCenter
  | AnchorEnd
  deriving (Show, Read)

instance ToJSON Anchor where
  toJSON = Aeson.String . T.toLower . fromMaybe "" . T.stripPrefix "Anchor" . T.pack . show

data Align
  = AlignCenter
  | AlignStart
  | AlignEnd
  | AlignRight
  | AlignBottom
  | AlignLeft
  | AlignTop
  deriving (Show, Read)

instance ToJSON Align where
  toJSON = Aeson.String . T.toLower . fromMaybe "" . T.stripPrefix "Align" . T.pack . show

--------------------------------------------------------------------------------

data Plugin
  = TitlePlugin Text
  | LegendPlugin Bool
  | DataLabelsPlugin Anchor Align
  deriving (Show)

pluginName :: (IsString a) => Plugin -> a
pluginName (TitlePlugin _) = "title"
pluginName (LegendPlugin _) = "legend"
pluginName (DataLabelsPlugin _ _) = "datalabels"

pluginsToJSON :: [Plugin] -> Aeson.Value
pluginsToJSON = Aeson.object . fmap (\p -> pluginName p .= p)

instance ToJSON Plugin where
  toJSON (TitlePlugin title) =
    Aeson.object ["display" .= True, "text" .= title]
  toJSON (LegendPlugin display) =
    Aeson.object ["display" .= display]
  toJSON (DataLabelsPlugin anchor align) =
    Aeson.object ["anchor" .= anchor, "align" .= align]

--------------------------------------------------------------------------------

data BarOptions = BarOptions
  { barIndexAxis :: Axis,
    barSkipNull :: Bool,
    barScales :: Scales,
    barPlugins :: [Plugin],
    barMaintainAspectRatio :: Bool
  }
  deriving (Show)

instance ToJSON BarOptions where
  toJSON BarOptions {..} =
    Aeson.object
      [ "indexAxis" .= barIndexAxis,
        "skipNull" .= barSkipNull,
        "scales" .= barScales,
        "plugins" .= pluginsToJSON barPlugins,
        "maintainAspectRatio" .= barMaintainAspectRatio
      ]

data LineOptions = LineOptions
  { lineIndexAxis :: Axis,
    linePlugins :: [Plugin]
  }
  deriving (Show)

instance ToJSON LineOptions where
  toJSON LineOptions {..} =
    Aeson.object
      [ "indexAxis" .= lineIndexAxis,
        "plugins" .= pluginsToJSON linePlugins
      ]

data PieOptions = PieOptions
  { pieRotation :: Int,
    piePlugins :: [Plugin]
  }
  deriving (Show)

instance ToJSON PieOptions where
  toJSON PieOptions {..} =
    Aeson.object
      [ "rotation" .= pieRotation,
        "plugins" .= pluginsToJSON piePlugins
      ]

data DoughnutOptions = DoughnutOptions
  { doughnutRotation :: Int,
    doughnutPlugins :: [Plugin]
  }
  deriving (Show)

instance ToJSON DoughnutOptions where
  toJSON DoughnutOptions {..} =
    Aeson.object
      [ "rotation" .= doughnutRotation,
        "plugins" .= pluginsToJSON doughnutPlugins
      ]

--------------------------------------------------------------------------------

data ChartData a = ChartData
  { dataLabels :: [Text],
    dataSets :: [DataSet a]
  }
  deriving (Show)

data DataSet a = DataSet
  { dataSetValues :: [Maybe a],
    dataSetLabel :: Text,
    dataSetBorderColors :: [Text],
    dataSetBackgroundColors :: [Text],
    dataSetBorderWidth :: Int,
    dataSetFill :: Bool
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (DataSet a) where
  toJSON DataSet {..} =
    Aeson.object
      [ "data" .= toJSON dataSetValues,
        "label" .= dataSetLabel,
        "borderColor" .= dataSetBorderColors,
        "backgroundColor" .= dataSetBackgroundColors,
        "borderWidth" .= dataSetBorderWidth,
        "fill" .= dataSetFill
      ]

--------------------------------------------------------------------------------

data Scales = Scales
  { scalesAxisX :: Maybe AxisOptions,
    scalesAxisY :: Maybe AxisOptions
  }
  deriving (Show)

instance ToJSON Scales where
  toJSON Scales {..} =
    Aeson.object $
      maybe [] (\o -> ["x" .= o]) scalesAxisX
        <> maybe [] (\o -> ["y" .= o]) scalesAxisY

data AxisOptions = AxisOptions
  { axisType :: Text,
    axisBeginAtZero :: Bool
  }
  deriving (Show)

instance ToJSON AxisOptions where
  toJSON AxisOptions {..} =
    Aeson.object
      [ "type" .= axisType,
        "beginAtZero" .= axisBeginAtZero
      ]

--------------------------------------------------------------------------------
