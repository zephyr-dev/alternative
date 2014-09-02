{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate, take, zip, dropWhile, isPrefixOf)
import System.Environment (getArgs)
import System.FilePath.Posix (splitPath, splitFileName, takeBaseName, splitExtension)
import Data.Yaml (decode)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import qualified Data.ByteString as BS

type FileName = String
type DirName = String
data Projection = Projection {
    source  :: [DirName]
  , target  :: [DirName]
  }

instance FromJSON Projection where
    parseJSON (Object v) = Projection <$>
                           (splitDirs <$> v .: "source") <*>
                           (splitDirs <$> v .: "target")
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Projection from YAML/JSON"

splitDirs = map (filter (/='/')) . splitPath

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      maybeProjections <- decode <$> BS.readFile "projections.yml"
      putStr $ maybe
        "bad yaml"
        (show . alternative filePath)
        maybeProjections

    _ -> putStr "wrong arguments"

  where
    alternative :: FilePath -> [Projection] -> FilePath
    alternative filePath projections =
      let
        alternativePath = joinDirs $ alternativeDirs (dirs filePath) projections
        dirs = splitDirs . fst . splitFileName 
        joinDirs = intercalate "/"
      in
      alternativePath ++ "/" ++ (alternativeFile filePath)
      where
        alternativeFile :: FilePath -> FileName
        alternativeFile filePath =
          let
            baseFilename = takeBaseName filePath
            (_, extension) = splitExtension fileName
            (_, fileName) = splitFileName filePath
          in
          baseFilename ++ "_spec" ++ extension

        alternativeDirs :: [DirName] -> [Projection] -> [DirName]
        {- alternativeDirs dirs [] = dirs -}
        alternativeDirs dirs (projection:rest) =
          maybe
            (alternativeDirs dirs rest)
            (target projection ++)
            $ matchProjection dirs projection


          where
            matchProjection :: [DirName] -> Projection -> Maybe [DirName]
            matchProjection dirs projection =
              let
                projectionSource = source projection
              in
              if projectionSource `isPrefixOf` dirs
              then Just $ map snd $ dropWhile (\(a, b) -> a == b) $ zipWithDefault "" "" projectionSource dirs
              else Nothing
              where
                zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
                zipWithDefault da db la lb = let len = max (length la) (length lb)
                                                 la' = la ++ (repeat da)
                                                 lb' = lb ++ (repeat db)
                                             in take len $ zip la' lb'  


