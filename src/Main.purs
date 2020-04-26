module Main where

import Prelude

import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view, over, lens, Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Effect (Effect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = { activeColors :: ActiveColors }


initialState ::  forall i. i -> State
initialState _ = 
  { activeColors: 
    { white: false
    , blue: false
    , black: false
    , red: false
    , green: false
    , colorless: false
    }
  }

data Card = ASDF

data BasicColor 
  = White
  | Blue
  | Black
  | Red
  | Green
  | Colorless

derive instance genericBasicColor :: Generic BasicColor _

instance showBasicColor :: Show BasicColor where
  show = genericShow

data Color 
  = Basic BasicColor
  | Combined BasicColor BasicColor
  | Generic

type ColorCost = { color :: Color
                 , amount :: Int
                 }

type ManaCost = Array ColorCost

-- "2{W/U}{B}"
example :: ManaCost
example = 
  [ {color: Generic, amount: 2}
  , {color: Combined White Blue, amount: 1}
  , {color: Basic Black, amount: 1}
  ]

type CardColors = Array Color

manaCostToColors :: ManaCost -> CardColors
manaCostToColors manaCost = map (_.color) manaCost

type ActiveColors =
   { white :: Boolean
   , blue :: Boolean
   , black:: Boolean
   , red :: Boolean
   , green :: Boolean
   , colorless :: Boolean
   }

canCast :: ActiveColors -> CardColors -> Boolean
canCast activeColors colors = all canCastColor colors
  where canCastBasicColor :: BasicColor -> Boolean
        canCastBasicColor = case _ of
          White -> activeColors.white
          Blue -> activeColors.blue
          Black -> activeColors.black
          Red -> activeColors.red
          Green -> activeColors.green
          Colorless -> activeColors.colorless
        canCastColor :: Color -> Boolean
        canCastColor = case _ of
          Generic -> true
          Basic color -> canCastBasicColor color
          Combined c1 c2 -> canCastBasicColor c1 || canCastBasicColor c2

data Action = Toggle BasicColor

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- [(_.black, Black), (_.blue, Blue)]
lensFor :: BasicColor -> Lens' ActiveColors Boolean
lensFor = case _ of
  White -> lens _.white (\ac b -> ac {white = b})
  Blue -> lens _.blue (\ac b -> ac {blue = b})
  Black -> lens _.black (\ac b -> ac {black = b})
  Red -> lens _.red (\ac b -> ac {red = b})
  Green -> lens _.green (\ac b -> ac {green = b})
  Colorless -> lens _.colorless (\ac b -> ac {colorless = b})

_activeColors :: Lens' State ActiveColors
_activeColors = lens _.activeColors (\st ac -> st {activeColors = ac})

allBasicColors :: Array BasicColor
allBasicColors = [White, Blue, Black, Red, Green, Colorless] --TODO: enumilla tms

renderBool :: forall m. ActiveColors -> BasicColor -> H.ComponentHTML Action () m
renderBool activeColors color 
  = HH.div [ HE.onClick \_ -> Just (Toggle color)] 
           [ HH.text $ (show color) <> ": " <> content ]
  where content = case view (lensFor color) activeColors of
          true -> "ON"
          false -> "OFF"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [] colors
  where colors = 
          map (renderBool state.activeColors) allBasicColors
  -- HH.div
  --   [ HE.onClick \_ -> Just DoSomething ]
  --   [ HH.text "hei" ]

-- _activeColors :: forall a r. Lens' { activeColors :: a | r } a
-- _black        :: forall a r. Lens' { black        :: a | r } a
-- _activeColors = prop
-- _black = prop


handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction (Toggle color) = H.modify_ \st -> over (_activeColors <<< lensFor color) not st

main :: Effect Unit  
main = HA.runHalogenAff do 
  body <- HA.awaitBody 
  runUI component initialState body
