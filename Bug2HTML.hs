module Bug2HTML where

import qualified System.IO.Unsafe as SIU

import qualified BugData as BD
import qualified BugXML2HS as BX2HS
import qualified Data.List as DL
import qualified HTMLCommon as H
import qualified Data.Char as DC
import qualified Data.Time as DT
import qualified System.Locale as SL
import qualified System.IO as SI


replaceHexValues :: String -> String
replaceHexValues [] = []
replaceHexValues ('&':'#':'x':c:';':xs) = (DC.chr d) : replaceHexValues xs
                 where d = read ['0','x',c] :: Int
replaceHexValues (x:xs) = x : replaceHexValues xs

applyBRTags :: String -> String
applyBRTags [] = []
applyBRTags ('\r':'\n':xs) = '<':'b':'r':'>': applyBRTags xs
applyBRTags ('\n':xs) = '<':'b':'r':'>': applyBRTags xs
applyBRTags ('\r':xs) = '<':'b':'r':'>': applyBRTags xs
applyBRTags (x:xs) = x : applyBRTags xs

cleanUpString :: String -> String
cleanUpString  = applyBRTags . replaceHexValues

priSevStr :: Int -> Int -> String
priSevStr priority severity = H.encloseInDIV className str
          where
               str = "[" ++ (show priority) ++ "/" ++ (show severity) ++ "]"
               className = (if priority >= 3 then "HighPriority" else "LowPriority")
                           ++ (if severity >= 3 then "HighSeverity" else "LowSeverity")
               
bugLink :: BD.Bug -> String
bugLink bug = "<a href=\"" ++ (show bugId) ++ ".html\" title=\""++ alterText ++ "\">" ++ (show bugId) ++ "</a>" ++ "<a href=\"http://company/#bug=" ++ (show bugId) ++ "\" target=\"body\"> (w) </a>"
        where
                bugId = BD.bugId bug
                alterText = "Dev = "
                            ++ BD.bugDev bug
                            ++ "\nQE = "
                            ++ BD.bugQE bug
                            ++ "\nStatus = "
                            ++ BD.bugStatus bug
                            ++ "\nState = "
                            ++ BD.bugState bug
                            ++ "\nReason = "
                            ++ BD.bugReason bug
                            ++ "\nVersion = "
                            ++ BD.bugVersion bug
                            ++ "\nCreated  = "
                            ++ show (BD.bugDateCreated bug)
                            ++ "\nUpdated  = "
                            ++ show (BD.bugDateUpdated bug)
                            ++ "\nAttachments = "
                            ++ (concat (map (\a -> (BD.userFileName a) ++ " ") (BD.bugAttachments bug)))
                            ++ "\nBuggroup relation = "
                            ++ BD.bugGroupRelationship bug
                           

getBugGroupString :: BD.Bug -> String
getBugGroupString bug = result  where
  result = case result' of
           [] -> []
           _ -> "Grouped with " ++ result'
  result' = concat $ map f bugs
  f g =  (show (BD.bugGroupBugId g)) ++ " " ++ (BD.bugGroupProduct g) ++ (priSevStr (BD.bugGroupPriority g) (BD.bugGroupSeverity g)) ++ (BD.bugGroupStatus g) ++ "; " 
  bugs = case bugs' of
         [] -> []
         (x:xs) -> xs
  bugs' = BD.bugGroupBugs bug
                

bug2htmlRecord :: DT.UTCTime -> BD.Bug -> String
bug2htmlRecord currentTime bug = H.encloseInDIV recordDivID $!
                        bugLink bug
                     ++ "  "
                     ++ priSevStr (BD.bugPriority bug) (BD.bugSeverity bug)
                     ++ "  "
                     ++ (BD.bugVersion bug)
                     ++ "  "
                     ++ (getBugGroupString bug)
                     ++ "<br><b>"
                     ++ (BD.bugStatus bug)
		     ++ " "
                     ++ (BD.bugReason bug)
		     ++ " "
                     ++ (BD.bugFixByMilestone bug)
		     ++ "</b> "
                     ++ (show diff) ++ " days since last update"
                     ++ "<br>"
                     ++ H.encloseInDIV "title" (BD.bugTitle bug)
               where
                    dev = BD.bugDev bug
                    qe = BD.bugQE bug
                    recordDivID = statusColor $ BD.bugStatus bug 
                    bugUpdateTime = DT.localTimeToUTC (DT.TimeZone 330 True "IST") (BD.bugDateUpdated bug)
                    diff = floor $ ( (DT.diffUTCTime currentTime bugUpdateTime) / (60*60*24))


statusColor :: String -> String
statusColor "ToFix" = "recordRed"
statusColor "NeedsReview" = "recordBlue"
statusColor "ToBuild" = "recordGreen"
statusColor _ = "record"


bug2html :: BD.Bug -> String
bug2html bug = H.htmlStart ++==++ body ++==++ H.htmlEnd
         where body =   H.encloseInDIV "title" ((bugLink  bug) ++==++ BD.bugTitle bug)
                     ++==++ H.encloseInDIV "note" ("<u>Summary</u><br>Dev=" ++==++ dev ++==++ " and QE=" ++==++ qe ++==++  "  " ++==++ attachmentLink ++==++ "<br>" ++==++ (getBugGroupString bug) ++==++ "<br><br>" ++==++ (cleanUpString $ BD.bugDescription bug))
                     ++==++ bugNotes2HTML (BD.bugNotes bug)
               dev = "<a href=\"report_dev_" ++==++ (BD.bugDev bug) ++==++ ".html\">" ++==++ (BD.bugDev bug) ++==++ "</a>"
               qe = "<a href=\"report_qe_" ++==++ (BD.bugQE bug) ++==++ ".html\">" ++==++ (BD.bugQE bug) ++==++ "</a>"
               attachmentLength = length (BD.bugAttachments bug)
               attachmentLink  = if attachmentLength > 0 then "<a href=\"attachments/" ++==++ (show (BD.bugId bug)) ++==++ "\"> (atachments) </a>"
                                   else ""
               (++==++) x y = ($!) (++) x y 



bugNotes2HTML :: [BD.BugNote] -> String
bugNotes2HTML [] = ""
bugNotes2HTML (n:ns) = (bugNote2HTML n) ++ bugNotes2HTML ns

bugNote2HTML :: BD.BugNote -> String
bugNote2HTML note = H.encloseInDIV "note" $
                       H.encloseInDIV "addedBy" (addedBy ++ date)
                    ++ H.encloseInDIV "noteString" ( (cleanUpString noteString))
                    where addedBy = BD.noteAddedBy note
                          noteString = BD.noteString note
                          date = "[" ++ (show $ BD.noteDate note) ++ "]"


writeToFile file str = do
            handle <- SI.openFile file SI.WriteMode
            SI.hPutStr handle str
            SI.hClose handle
            

mapMI :: (a -> IO b) -> [a] -> IO [b]
mapMI _ [] = return [] -- You can play with this case a bit. This will open a file for the head of the list,
-- and then when each subsequent cons cell is inspected. You could probably
-- interleave 'f x' as well.
mapMI f (x:xs) = do y <- SIU.unsafeInterleaveIO (f x) ; ys <- SIU.unsafeInterleaveIO (mapMI f xs) ; return (y:ys)



dumpHTML :: [BD.Bug] -> IO ([BD.Bug])
dumpHTML bugs = mapMI dumpHTML' bugs 

dumpHTML' :: BD.Bug -> IO (BD.Bug)
dumpHTML' b = do
         let str = ($!) bug2html b
         let bugId = BD.bugId b
         let htmlFile = "html/" ++ (show bugId) ++ ".html"
         putStrLn $ "Writing " ++ htmlFile
         SI.hFlush SI.stdout
         writeToFile htmlFile str
         return b

