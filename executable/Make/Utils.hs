module Make.Utils ( (!!?)
                  , withPath,
                  pathIdx
                  , pathIdx'
                  , withReverse
                  , getDirectoryFilesWithExt
                  , stripSuffix
                  , splitBy
                  , fieldEq ) where

import Data.List
import Development.Shake.FilePath
import System.Directory

(!!?) :: [a] -> Int -> Maybe a
as !!? a = if a < 0 then
             Nothing
           else
             search as a
  where search (x:_) 0 = Just x
        search (_:xs) n = search xs (n-1)
        search []     _ = Nothing

withPath :: ([FilePath] -> [FilePath]) -> FilePath -> FilePath
withPath f = joinPath . f . splitDirectories

pathIdx :: FilePath -> Int -> FilePath
pathIdx p i = (splitDirectories p) !! i

pathIdx' :: FilePath -> Int -> FilePath
pathIdx' p i = split !! (top - i)
  where split = splitDirectories p
        top = length split - 1

withReverse :: ([a] -> [b]) -> [a] -> [b]
withReverse f = reverse . f . reverse

getDirectoryFilesWithExt :: FilePath -> String -> IO [FilePath]
getDirectoryFilesWithExt dir ext = do
  files <- getDirectoryContents dir
  let files' = filter ((==ext) . takeExtension) files
  return $ map (dir </>) files'

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix = (reverse <$>) . stripPrefix (reverse suffix) . reverse

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy p ls = case break p ls of
                 (prefix, []) -> [prefix]
                 (prefix, _:ls') -> prefix : splitBy p ls'

fieldEq :: Eq b => (a -> b) -> a -> a -> Bool
fieldEq f x1 x2 = (f x1) == (f x2)
