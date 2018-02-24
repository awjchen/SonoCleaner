-- The 'Model' of "Model, View, Controller"
--
-- In this program, the Model is responsible for handling the data manipulated
-- by the program, that is, sonomicrometry distance traces from .ssa files. This
-- includes providing accessors to and operators on the data, loading and
-- initializing data from .ssa files, and writing that data back to .ssa files.

-- This file defines the interface of the Model to the rest of the program.
-- Modules outside of the Model should only access the Model by importing this
-- module.

module Model
  ( module M
  ) where

import           Model.Default            as M
import           Model.InterpolationBrush as M
import           Model.Labelling          as M
import           Model.Matching           as M
import           Model.Model              as M
import           Model.TraceOperators     as M
import           Model.TraceState         as M
