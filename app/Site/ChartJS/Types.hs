{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Site.ChartJS.Types where

import Data.Aeson as Aeson
import qualified Data.Char as Char
import Data.List (stripPrefix)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import GHC.Generics

--------------------------------------------------------------------------------

data TableData = TableData [Text] [[(Text, Text)]]

--------------------------------------------------------------------------------

data Chart a = Chart
  { cName :: Text,
    cTitle :: Maybe Text,
    cHeight :: Maybe Text,
    cWidth :: Maybe Text,
    cType :: Text,
    cDisplayLegend :: Bool,
    cScales :: ChartScales,
    cData :: ChartData a
  }
  deriving (Show)

data ChartData a = ChartData
  { cdLabels :: [Text],
    cdDataSets :: [ChartDataSet a]
  }
  deriving (Show)

data ChartDataSet a = ChartDataSet
  { cdsData :: [Maybe a],
    cdsLabel :: Text,
    cdsBorderColors :: [Text],
    cdsBackgroundColors :: [Text],
    cdsBorderWidth :: Int,
    cdsFill :: Bool
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (ChartDataSet a) where
  toJSON ChartDataSet {..} =
    Aeson.object
      [ "data" .= toJSON cdsData,
        "label" .= cdsLabel,
        "borderColor" .= cdsBorderColors,
        "backgroundColor" .= cdsBackgroundColors,
        "borderWidth" .= cdsBorderWidth,
        "fill" .= cdsFill
      ]

  toEncoding =
    genericToEncoding $
      defaultOptions
        { fieldLabelModifier = stripCamelCasePrefix "lds"
        }

--------------------------------------------------------------------------------

data ChartScales = ChartScales
  { csAxisX :: Maybe ChartAxis,
    csAxisY :: Maybe ChartAxis
  }
  deriving (Show)

data ChartAxis = ChartAxis
  { caType :: Text,
    caBeginAtZero :: Bool
  }
  deriving (Show)

instance ToJSON ChartScales where
  toJSON ChartScales {..} =
    Aeson.object
      [ "x" .= toJSON (maybeToList csAxisX),
        "y" .= toJSON (maybeToList csAxisY)
      ]

instance ToJSON ChartAxis where
  toJSON ChartAxis {..} =
    Aeson.object
      [ "type" .= caType,
        "ticks"
          .= Aeson.object
            [ "beginAtZero" .= caBeginAtZero
            ]
      ]

--------------------------------------------------------------------------------

stripCamelCasePrefix :: String -> String -> String
stripCamelCasePrefix prefix label =
  case stripPrefix prefix label of
    Nothing -> label
    Just (h : t) -> Char.toLower h : t
    Just t -> t
