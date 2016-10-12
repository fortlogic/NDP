module Make.Utils ((!!?),
                  withPath,
                  pathIdx,
                  pathIdx',
                  withReverse) where

import Development.Shake.FilePath

(!!?) :: [a] -> Int -> Maybe a
as !!? a = if a < 0 then
             Nothing
           else
             search as a
  where search (a:as) 0 = Just a
        search (a:as) n = search as (n-1)
        search []     _ = Nothing

withPath :: ([FilePath] -> [FilePath]) -> FilePath -> FilePath
withPath f = joinPath . f . splitDirectories

pathIdx :: FilePath -> Int -> FilePath
pathIdx p i = (splitDirectories p) !! i

pathIdx' :: FilePath -> Int -> FilePath
pathIdx' p i = split !! (max - i)
  where split = splitDirectories p
        max = length split - 1

withReverse :: ([a] -> [b]) -> [a] -> [b]
withReverse f = reverse . f . reverse
