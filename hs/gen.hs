import Data.List.Split {- split -}
import System.Random {- random -}
import Text.Printf {- base -}

import qualified Sound.SC3.Lang.Random.Gen as L {- hsc3-lang -}

import qualified JRD as J {- ut/www-jrd -}

img_offsets :: [[Int]]
img_offsets = chunksOf 4 (L.s_rrand 0 100 (mkStdGen 246873))

mk_padding :: [Int] -> String
mk_padding p =
    case p of
      [p0,p1,p2,p3] -> printf "padding: %dpx %dpx %dpx %dpx" p0 p1 p2 p3
      _ -> error "mk_padding?"

img_id_rand_css :: Int -> [Int] -> String
img_id_rand_css n p = printf "div.%s {%s}" (J.img_id n) (mk_padding p)

gen :: IO ()
gen = putStrLn $ unlines $ zipWith img_id_rand_css [0..200] img_offsets
