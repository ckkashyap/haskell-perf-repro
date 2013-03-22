import qualified System.Directory as D
import qualified System.IO.Unsafe as SIU
import qualified BugData as BD
import qualified BugXML2HS as BX2HS
import qualified Data.List as DL
import RSXP
import qualified Report as R


import qualified Bug2HTML as BH
import qualified System.IO as SI
import qualified Data.ByteString.Lazy.Char8 as B


outputDir = "data"
htmlDir = "html"
rawExt = ".xml"

removeDir :: String -> IO ()
removeDir path = do
  e <- D.doesDirectoryExist path
  if e then (D.removeDirectoryRecursive path) else return ()

renameDir :: String -> String -> IO ()
renameDir p1 p2 = do
  removeDir p2
  e <- D.doesDirectoryExist p1
  if e then D.renameDirectory p1 p2 else return ()
  
createDir dir = do
           removeDir dir
           D.createDirectory dir



mapMI :: (a -> IO b) -> [a] -> IO [b]
mapMI _ [] = return [] -- You can play with this case a bit. This will open a file for the head of the list,
-- and then when each subsequent cons cell is inspected. You could probably
-- interleave 'f x' as well.
mapMI f (x:xs) = do y <- SIU.unsafeInterleaveIO (f x) ; ys <- SIU.unsafeInterleaveIO (mapMI f xs) ; return (y:ys)


fileContents2Bug :: String -> BD.Bug
fileContents2Bug str =bug
   where
        str' = (unlines . drop 4 . lines) str
        xml =  parseXML str'
        bug = case ((BX2HS.parseBug xml) `seq` (BX2HS.parseBug xml)) of
                     Just something -> something
                     Nothing -> BD.badBug



  
getBugs :: IO [BD.Bug]
getBugs = do
        files <- D.getDirectoryContents outputDir
        let xmlFiles = map (\n -> outputDir ++ "/" ++ n) $
                       filter (\n -> (reverse . take 4 . reverse) n == rawExt) files
        putStrLn (show xmlFiles)
        mapMI (\f -> do {str <- readFile f; return (fileContents2Bug str); }) (xmlFiles)






main = do
     bugs <- getBugs
     bugs <- BH.dumpHTML bugs
     mapM_ (\b -> putStrLn (show $ BD.bugId b)) bugs
     R.report bugs

     return ()
     
  
  


