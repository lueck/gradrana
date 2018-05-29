{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} Test.GraDrAna.Graph.CoPresence
import {-@ HTF_TESTS @-} Test.GraDrAna.Graph.TurnQuantity

main = htfMain htf_importedTests
