module Utils where

import Data.Maybe

update :: Int -> a -> [a] -> [a]
update i x = map (\(i', x') -> if i == i' then x else x') . zip [0..]

selects :: Int -> [a] -> [[a]]
selects 0 _ = []
selects 1 ws = map pure ws
selects n ws = concatMap (\c -> map (c:) $ selects (n - 1) ws) ws

perms :: [(Int, [a])] -> [[a]]
perms objs = check $ mapMaybe takeOne [0..length objs - 1]
  where takeOne i
          | c == 0 = Nothing
          | otherwise = Just ((c, ws), update i (c - 1, ws) objs)
          where (c, ws) = objs !! i

        check [((c, ws), _)] = selects c ws
        check x = concatMap permut x
        
        permut ((_, ws), objs') = concatMap (\c -> map (c :) $ perms objs') ws
