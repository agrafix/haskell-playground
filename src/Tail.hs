{-# LANGUAGE BangPatterns #-}
module Tail where

import Data.Sequence ((|>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import System.IO

showTail :: [BSC.ByteString] -> IO ()
showTail bs =
    mapM_ BSC.putStrLn bs

runTail :: FilePath -> Integer -> IO [BSC.ByteString]
runTail fp lines =
    do hdl <- openFile fp ReadMode
       hSeek hdl SeekFromEnd 0
       bytes <- readLoop hdl (lines + 1)
       return $ BSC.split '\n' (BSC.concat $ F.toList bytes)
    where
      newline = BSC.singleton '\n'
      readLoop hdl !ct
          | ct == 0 = return Seq.empty
          | otherwise =
              do currentPos <- hTell hdl
                 case currentPos of
                   0 -> return Seq.empty
                   1 -> do hSeek hdl AbsoluteSeek 0
                           bs <- BSC.hGet hdl 1
                           return (Seq.singleton bs)
                   _ -> do hSeek hdl AbsoluteSeek (currentPos - 1)
                           bs <- BSC.hGet hdl 1
                           hSeek hdl AbsoluteSeek (currentPos - 1)
                           let ct' = if bs == newline then ct - 1 else ct
                           xs <- readLoop hdl ct'
                           if ct' == 0 && bs == newline
                           then return xs
                           else return (xs |> bs)
