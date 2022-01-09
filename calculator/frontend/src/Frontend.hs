{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

import Reflex
import Reflex.Dom 
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack, unpack, Text, empty)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import Control.Monad.Fix (MonadFix)

import Crypto.Hash (hashlazy, Digest, SHA3_512, hash)
import Data.ByteString.Char8 as C8 (pack)
import Data.ByteArray
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS


import Data.Time
import Control.Monad.Trans (liftIO)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
    tutorial1
    tutorial2
    tutorial3
    tutorial4
    tutorial5
    tutorial6
    tutorial7
    tutorial8
    tutorial9
    return()      
  }


tutorial1 :: DomBuilder t m => m ()
tutorial1 = el "div" $ text "Welcome to Reflex"

-- el (HTML Tag) (Text widget to show)
-- Generates Line of Text ^

tutorial2 :: DomBuilder t m => m ()
tutorial2 = el "div" $ do
  el "p" $ text "Reflex is:"
  el "ul" $ do
    el "li" $ text "Efficient"
    el "li" $ text "Higher-order"
    el "li" $ text "Glitch-free"

-- Generation of Text with different Tags ^

tutorial3 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial3 = el "div" $ do
  t <- inputElement def
  text " "
  dynText $ hashVal $ _inputElement_value t

-- Generation Of inputElement with hashing Algo Text ^

hashVal :: (Reflex t) => Dynamic t Text -> Dynamic t Text
hashVal x = 
  fmap (\y -> hashX y) x

-- _inputElement_value returns the current Text value of the inputElement
-- Text is contained inside (Dynamic t) container
-- fmap is used to modify the Text ^ 

hashX :: Text -> Text
hashX t  
  | T.unpack t == "" = T.pack "  Enter String To Hash"
  | otherwise =  T.pack $ shaLazy $ C8.pack $ T.unpack t 
  
shaLazy :: BS.ByteString -> String
shaLazy bs = show (hash bs :: Digest SHA3_512)

-- Modifing the Text (Hashing Algorithm) and returning the hash ^

tutorial4 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial4 = el "div" $ do
  t <- inputElement $ def
    & inputElementConfig_initialValue .~ "0"
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
  text " "
  dynText $ _inputElement_value t

-- Creating An inputElement Configured with limited input of type number
-- And showing the input with dynText ^

tutorial5 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial5 = el "div" $ do 
  x <- numberInput
  let numberString = fmap (T.pack . show) x
  text " "
  dynText numberString
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . T.unpack) $ _inputElement_value n

-- Tutorial 5 is about making an inputElement and ensuring it's number type
-- function numberInput returns value of Maybe Double that means :
-- if value is a number we get - Just (number)
-- otherwise we get - Nothing

tutorial6 :: (DomBuilder t m, PostBuild t m) => m ()
tutorial6 = el "div" $ do
  nx <- numberInput
  text " + "
  ny <- numberInput
  text " = "
  let result = zipDynWith (\x y -> (+) <$> x <*> y) nx ny
      resultString = fmap (T.pack . show) result
  dynText resultString
  where 
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do 
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . T.unpack) $ _inputElement_value n

-- Tutorial 6 is the collection of two numberInput (Just Like From Tutorial5)
-- Adding the String " + " and " = "
-- And Showing the sum of those 
-- Using zipDynWith to modify the value and saving to result

-- TUTORIAL 7 and on :

data Op = Plus | Minus | Times | Divide
  deriving (Eq, Ord, Show)

-- Math Operator Enumerator ^

runOp :: Fractional a => Op -> a -> a -> a
runOp s = case s of 
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Divide -> (/)

-- Function Implementing Operators Enum^

ops :: Map Op Text
ops = Map.fromList [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

-- Mapping Op Enum to their symbol

tutorial7 :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
tutorial7 = el "div" $ do
  nx <- numberInput
  op <- _dropdown_value <$> dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) op values
      resultText = fmap (T.pack . show) result
  text " = "
  dynText resultText
  where
    numberInput :: DomBuilder t m => m (Dynamic t (Maybe Double))
    numberInput = do 
      n <- inputElement $ def 
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return . fmap (readMaybe . T.unpack) $ _inputElement_value n


-- Just like Tutorial6 plus we got a dropdown containing the map of the operators (op)
-- values takes number inputs nx and ny and construct to a tupple
-- result performe the current operator shown in dropdown to the inner values of nx and ny
-- resultText converts result to Text
-- text is added of the " = " symbol
-- and finally dynText displaying the value of resultText 

-- Number Pad - Tutorial8 and On :

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass c s = do
  (e, _) <- elAttr' "button" ("type" =: "button" <> "class" =: c) $ text s
  return $ domEvent Click e
-- Build Button with Event 


numberPad :: (DomBuilder t m) => m (Event t Text)
numberPad = do 
  b7 <- ("7" <$) <$> numberButton "7"
  b8 <- ("8" <$) <$> numberButton "8"
  b9 <- ("9" <$) <$> numberButton "9"
  b4 <- ("4" <$) <$> numberButton "4"
  b5 <- ("5" <$) <$> numberButton "5"
  b6 <- ("6" <$) <$> numberButton "6"
  b1 <- ("1" <$) <$> numberButton "1"
  b2 <- ("2" <$) <$> numberButton "2"
  b3 <- ("3" <$) <$> numberButton "3"
  b0 <- ("0" <$) <$> buttonClass "number zero" "0"
  return $ leftmost [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9]
  where
    numberButton n = buttonClass "number" n

-- we use double layer of fmap to turn the result into (m (Event t Text))
-- ("0" <$) applied to the Event inside m 
-- then <$> to replace () inside Event to (Event t "0")

-- leftmost it used to funnel all the events to Event channel.
-- leftmost :: Reflex t => [Event t a] -> Event t a


tutorial8 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial8 = el "div" $ do
  numberButton <- numberPad
  clearButton <- button "C"
  let buttons = leftmost 
        [Nothing <$ clearButton
        ,Just <$> numberButton ]
  dstate <- accumDyn collectButtonPresses initialState buttons
  text " "
  dynText dstate
  where
    initialState :: Text 
    initialState = T.empty

    collectButtonPresses :: Text -> Maybe Text -> Text
    collectButtonPresses state buttonPress = 
      case buttonPress of
        Nothing -> initialState
        Just digit -> state <> digit

-- Tutorial 8 ^
-- Implementing numberPad and adding clearButton
-- funnel the events to $buttons using leftmost and adding Maybe to the buttons
-- accumDyn collecting the Events created from the buttons and put it to Text (collectButtonPresses)
-- accumDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (Dynamic t a)
-- with collectButtonPresses we either clear the Text to initial state (which is empty Text) or we merge text to the given state
-- dynText shows the collected text


-- A Minimal Four Function Calculator:

data CalcState = CalcState 
  { _calcState_acc    :: Double   -- accumulator
  , _calcState_op     :: Maybe Op -- most recently requested operation
  , _calcState_input  :: Text     -- current input
  } deriving (Show)

-- Creating data type for the state of the calculator ^
 
data Button = ButtonNumber Text | ButtonOp Op | ButtonEq | ButtonClear

-- Sorting the types of buttons available on our calculator ^ 

initCalcState :: CalcState
initCalcState = CalcState 0 Nothing ""

-- Initializing the calculator with 0 accumulator, no operator and no text

updateCalcState :: CalcState -> Button -> CalcState
updateCalcState state@(CalcState acc mOp input) btn =
  case btn of 
    ButtonNumber d -> 
      if d == "." && T.find (== '.') input /= Nothing
        then state
        else CalcState acc mOp (input <> d) 
    -- ^ We either merge floating dot to our Text or we return the same Text (if no number exists in input)
    ButtonOp pushedOp -> applyOp state (Just pushedOp)
    -- ^ Applying the operator to the state of the calculator 
    ButtonEq -> applyOp state Nothing
    -- ^ Update the state of the calculator to show final state with no operators
    ButtonClear -> initCalcState
    -- ^ Update calculator state to initial state

applyOp :: CalcState -> Maybe Op -> CalcState
applyOp state@(CalcState acc mOp input) mOp' =
  if T.null input
    then CalcState acc mOp' input
    else
      case readMaybe (T.unpack input) of
        Nothing -> state -- this should be unreachable
        Just x -> case mOp of
          Nothing -> CalcState x mOp' ""
          Just op -> CalcState (runOp op acc x) mOp' ""


displayCalcState :: CalcState -> Text 
displayCalcState (CalcState acc _op input) =
  if T.null input
    then T.pack (show acc)
    else input
-- Either display accumlator (if no input exists)
-- Or we display the current input

tutorial9 :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
tutorial9 = el "div" $ do
  numberButtons <- numberPad
  bPeriod <- ("." <$) <$> button "."
  bPlus <- (Plus <$) <$> button "+"
  bMinus <- (Minus <$) <$> button "-"
  bTimes <- (Times <$) <$> button "*"
  bDivide <- (Divide <$) <$> button "/"
  let opButtons = leftmost [bPlus, bMinus, bTimes, bDivide]
  bEq <- button "="
  bClear <- button "C"
  let buttons = leftmost
        [ ButtonNumber <$> numberButtons
        , ButtonNumber <$> bPeriod
        , ButtonOp <$> opButtons
        , ButtonEq <$ bEq
        , ButtonClear <$ bClear
        ]
  calcState <- accumDyn updateCalcState initCalcState buttons
  text " "
  dynText (displayCalcState <$> calcState)

-- Tutorial 9 ^ 
-- Implementing all of the above functions with the same style of tutorial8







