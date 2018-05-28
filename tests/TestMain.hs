{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest

import {-@ HTF_TESTS @-} Test.GraDrAna.Graph.CoPresence

main = htfMain htf_importedTests
