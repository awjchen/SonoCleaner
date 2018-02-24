-- A default .ssa file to use before the user has opened one.
--
-- We store the default .ssa file as a string so that it may be included in the
-- binary itself. I'd prefer not to do things this way, but I don't know how
-- else to do this.

-- Note: be careful when automatically applying formatting to this file
-- because .ssa files end lines with whitespace (tab characters).

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Model.Default
  ( defaultModel
  , defaultModel'
  ) where

import qualified Data.Text         as T
import           Text.RawString.QQ (r)

import           Model.Model
import           Model.Ssa

defaultSsaFile :: T.Text
defaultSsaFile = [r|TITLE:	Trace Segment ASCII
VERSION:	3.10
CREATION DATE:	02/17/17
CREATION TIME:	10:32:53
PARENT FILE:	C:\default.slb
SAMPLE TIME INTERVAL:	0.002466
UNIT OF TIME INTERVAL:	second
# OF ROW:	3
# OF COLUMN:	2
INDEX COLUMN #:	1
BEGIN DATA:
Time	TRX01:02	
sec	mm		Delineators
0.000000	17.639063	
0.002466	17.639063	
0.004931	17.639063	
END DATA
|]

defaultModel' :: Either String Model
defaultModel' = do
  ssa <- parseSSA "" defaultSsaFile
  initModel "default.ssa" ssa [] 0

-- There is a test for the success of `fromRight`
defaultModel :: Model
defaultModel = fromRight defaultModel'

fromRight :: Either a b -> b
fromRight = either (error "fromRight") id
