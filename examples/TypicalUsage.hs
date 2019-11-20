{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import RON.Data
import RON.Data.RGA
import RON.Schema.TH
import RON.Storage.FS as Storage

[mkReplicated|
  (struct_set Note
    (active  lww   Bool)
    (text    merge RgaString))
|]

instance Collection Note where
  collectionName = "note"

main :: IO ()
main = do
  let dataDir = "./data/"
  h <- Storage.newHandle dataDir
  runStorage h $ do
    obj <-
      newObjectFrame
        Note
          { active = Just True
          , text = Just $ RGA "Write a task manager"
          }
    createDocument obj
