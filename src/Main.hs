{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate, take, zip, dropWhile, isPrefixOf)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (splitPath, splitFileName, takeBaseName, splitExtension)
import Data.Yaml (decode)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import qualified Data.ByteString as BS

type FileName = String
type DirName  = String
type DirPath  = [DirName]
data FullDirPath  = FullDirPath {
    pathPrefix :: DirPath
  , pathPostfix :: DirPath
  }
data KeepPrefix = KeepPrefix | DontKeepPrefix
type Path       = String
data Projection = Projection {
    source  :: DirPath
  , target  :: DirPath
  }

instance FromJSON Projection where
    parseJSON (Object v) = Projection <$>
                           (splitDirs <$> v .: "source") <*>
                           (splitDirs <$> v .: "target")
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _ = error "Can't parse Projection from YAML/JSON"

splitDirs   = map (filter (/='/')) . splitPath  :: Path -> [DirName]
unsplitDirs = intercalate "/"                   :: [DirName] -> Path

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      maybeProjections <- decode <$> BS.readFile "config/projections.yml"
      case maybeProjections of
        Nothing           -> error "bad yaml"
        Just projections  -> do
          let dirPathToEnsureExists = alternativePath filePath projections KeepPrefix
          createDirectoryIfMissing True dirPathToEnsureExists
          putStr $ alternative filePath projections
    _ -> putStrLn "wrong arguments"

  where
    dirs :: String -> DirPath
    dirs = splitDirs . fst . splitFileName

    alternativePath :: String -> [Projection] -> KeepPrefix -> String
    alternativePath filePath = toSinglePath . alternativeDirs (FullDirPath [] $ dirs filePath)
      where
        toSinglePath :: FullDirPath -> KeepPrefix -> Path
        toSinglePath (FullDirPath prefix postfix) cond = case cond of
          KeepPrefix      -> (unsplitDirs prefix) ++ "/" ++ (unsplitDirs postfix)
          DontKeepPrefix  -> unsplitDirs postfix

        addPrefixToPath :: DirPath -> FullDirPath -> FullDirPath
        addPrefixToPath dirs (FullDirPath prefixDirs postfixDirs) = FullDirPath prefixDirs (dirs ++ postfixDirs)

        alternativeDirs :: FullDirPath -> [Projection] -> FullDirPath
        alternativeDirs path [] = error $ "unknown source dir: " ++ (toSinglePath path DontKeepPrefix)
        alternativeDirs path (projection:projections) =
          maybe
            (alternativeDirs path projections)
            (prependFullDirPathPostfix $ target projection)
            $ matchProjection path projection

          where
            prependFullDirPathPostfix :: DirPath -> FullDirPath -> FullDirPath
            prependFullDirPathPostfix prepend path@FullDirPath{pathPostfix = post} = path{pathPostfix = prepend ++ post}

            matchProjection :: FullDirPath -> Projection -> Maybe FullDirPath
            matchProjection (FullDirPath _ []) projection = Nothing
            matchProjection (FullDirPath prefixDirs dirs@(dir:rest)) projection =
              let
                projectionSource = source projection
              in
              if projectionSource `isPrefixOf` dirs
                then Just $ FullDirPath prefixDirs $ map snd $ dropWhile (\(a, b) -> a == b) $ zipWithDefault "" "" projectionSource dirs
                else matchProjection (FullDirPath (prefixDirs++[dir]) rest) projection

                where
                  zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
                  zipWithDefault da db la lb = 
                    let
                      len = max (length la) (length lb)
                      la' = la ++ (repeat da)
                      lb' = lb ++ (repeat db)
                    in take len $ zip la' lb'


    alternative :: FilePath -> [Projection] -> FilePath
    alternative filePath projections = (alternativePath filePath projections DontKeepPrefix) ++ "/" ++ (alternativeFile filePath)
      where

        alternativeFile :: FilePath -> FileName
        alternativeFile filePath = baseFilename ++ "_spec" ++ extension
          where
            baseFilename = takeBaseName filePath
            (_, extension) = splitExtension fileName
            (_, fileName) = splitFileName filePath




