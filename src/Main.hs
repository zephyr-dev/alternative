{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (intercalate, take, zip, dropWhile, isPrefixOf)
import System.Environment (getArgs)
import System.FilePath.Posix (splitPath, splitFileName, takeBaseName, splitExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    filePath:[] -> do
      
      putStr $ show $ alternative filePath
    _ -> putStr "wrong rguments"

  where
    alternative filePath = 
      let
        alternativePath = intercalate "/" (alternativeDirs (splitDirs filePath) alternativeRules)
      in
       alternativePath ++ "/" ++ (alternativeFile filePath)

    alternativeFile filePath =
      let
        baseFilename = takeBaseName filePath
        (_, extension) = splitExtension fileName
        (_, fileName) = splitFileName filePath

      in
      baseFilename ++ "_spec" ++ extension

    splitDirs filePath =
      let
        (path, _) = splitFileName filePath
      in

      map (filter (/='/')) $ splitPath path


    alternativeDirs :: [String] -> [([String], [String])] -> [String]
    {- alternativeDirs dirs [] = dirs -}
    alternativeDirs dirs (rule:rest) =
      case matchRule dirs (fst rule) of
        Nothing ->
          alternativeDirs dirs rest
        Just remainder ->
          (snd rule) ++ remainder


    matchRule :: [String] -> [String] -> Maybe [String]
    matchRule dirs rulePrefix =
      if rulePrefix `isPrefixOf` dirs
      then Just $ map snd $ dropWhile (\(a, b) -> a == b) $ zipWithDefault "" "" rulePrefix dirs
      else Nothing

    zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
    zipWithDefault da db la lb = let len = max (length la) (length lb)
                                     la' = la ++ (repeat da)
                                     lb' = lb ++ (repeat db)
                                 in take len $ zip la' lb'  

    alternativeRules = [
          (["app", "assets", "javascripts"], ["spec", "javascripts"])
        , (["app"],                          ["spec", "zephyr"])
      ]
