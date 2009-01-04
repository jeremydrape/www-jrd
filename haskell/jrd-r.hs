import Data.List
import Data.Maybe
import Flickr
import System.Directory
import System.FilePath

key :: String
key = "fc835bdbc725d54415ff763ee93f7c2d"

user :: String
user = "28389435@N07"

write_database :: [Integer] -> IO ()
write_database ns =
    do let d = ".." </> "f" </> "db"
       createDirectoryIfMissing True d
       is <- mapM (fmap fromJust . get_info key) (map show ns)
       let f (n, i) = writeFile (d </> show n) (show i)
       mapM_ f (zip ns is)

rebuild :: IO ()
rebuild = do
  is <- get_public_photos key user 1 100
  write_database (map (read . identifier) is)
