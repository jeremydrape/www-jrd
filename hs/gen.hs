import Data.List {- base -}
import System.Random {- random -}
import Text.Printf {- base -}

import qualified Sound.Sc3.Lang.Random.Gen as L {- hsc3-lang -}

import qualified Jrd {- ut/www-jrd -}

-- > take 4 $ img_offsets (400, 200)
img_offsets :: (Int, Int) -> [[Int]]
img_offsets (h, w) =
    let t = L.s_rrand 0 h (mkStdGen 246873)
        r = L.s_rrand 0 w (mkStdGen 174027)
        b = L.s_rrand 0 h (mkStdGen 380984)
        l = L.s_rrand 0 w (mkStdGen 023664)
    in transpose [t,r,b,l]

mk_padding :: [Int] -> String
mk_padding p =
    case p of
      [p0,p1,p2,p3] -> printf "padding: %dpx %dpx %dpx %dpx" p0 p1 p2 p3
      _ -> error "mk_padding?"

img_id_rand_css :: Int -> [Int] -> String
img_id_rand_css n p = printf "div.%s {%s}" (Jrd.img_id n) (mk_padding p)

gen :: IO ()
gen = putStrLn $ unlines $ zipWith img_id_rand_css [0..200] (img_offsets (400, 200))
