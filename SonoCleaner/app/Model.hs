-- The 'Model' of "Model, View, Controller"
--
-- In this program, the Model is responsible for handling the data manipulated
-- by the program, that is, sonomicrometry distance traces from .ssa files. This
-- includes providing accessors to and operators on the data, loading and
-- initializing data from .ssa files, and writing that data back to .ssa files.

module Model
  ( module M
  ) where

import           Model.InterpolationBrush as M
import           Model.Labelling          as M
import           Model.Matching           as M
import           Model.Model              as M
import           Model.TraceOperators     as M
import           Model.TraceState         as M
