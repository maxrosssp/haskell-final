{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

module Main where

--import "fileshare-final" Application (develMain)
import Prelude (IO)

import Control.Concurrent.STM
import Data.IntMap
import Yesod

import Dispatch ()
import Foundation

main :: IO ()
main = do
    tstore <- atomically (newTVar empty)
    tident <- atomically (newTVar 0)
    warpEnv (App tident tstore)