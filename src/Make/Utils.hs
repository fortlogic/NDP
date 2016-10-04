module Make.Utils ((!!?),
                  withPath,
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

withReverse :: ([a] -> [b]) -> [a] -> [b]
withReverse f = reverse . f . reverse
