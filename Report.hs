module Report where

import qualified BugData as BD
import qualified BugXML2HS as BX2HS
import qualified Data.List as DL
import qualified HTMLCommon as H
import qualified Data.Time as DT
import qualified System.Locale as SL

import Bug2HTML

sortNgroup f bugs =
   DL.groupBy groupingFn $
   DL.sortBy sortingFn bugs
   where
     groupingFn b1 b2 = (f b1) == (f b2)
     sortingFn b2 b1 = compare (f b1) (f b2)



byPriority bugs = ( concat . concat ) $ map (sortNgroup BD.bugSeverity) (sortNgroup BD.bugPriority bugs)

byUpdateDate bugs = DL.sortBy f bugs
        where
                f b2 b1 = compare (BD.bugDateUpdated b1) (BD.bugDateUpdated b2)

byRole f bugs = (concat.concat.concat) $ (map.map) (sortNgroup BD.bugSeverity) $ map (sortNgroup BD.bugPriority) (sortNgroup f bugs) 

byDev = byRole BD.bugDev
byQE = byRole BD.bugQE

genSummaryReport :: String -> String -> [(String, Int, Int)] -> IO ()
genSummaryReport role fileName  tupleList = do
	let string = concat $ map (\(s, c1, c2) -> "<tr><td><a href=\"report_" ++ role ++ "_" ++ s ++ ".html\">" ++ s ++ "</a></td><td align=\"right\">" ++ (show c1) ++ "</td><td align=\"right\">" ++ (show c2) ++ "</td></tr>\n") tupleList
	writeFile fileName $ (H.htmlStart ++ "<table border=\"1\"><tr><th>Name</th><th>bug count</th><th>Actionable bug count</th></tr>\n" ++ string ++ "</table>" ++ H.htmlEnd)

genReport :: String -> ([BD.Bug] -> [BD.Bug]) -> [BD.Bug] -> IO ()
genReport fileName f bugs = do
  currentUTCTime <- DT.getCurrentTime
  let list = f bugs
  let string = concat $ map (\s -> (($!) (++) (($!) (++) "<li>"  s)) "</li>") $ map (bug2htmlRecord currentUTCTime) list
  writeFile fileName $ (H.htmlStart ++ "<ol>" ++ string ++ "</ol>" ++ H.htmlEnd)

byPriorityReport = genReport "html/report_by_priority.html" byPriority

byUpdateDateReport = genReport "html/report_by_update_date.html" byUpdateDate

byRoleReport role f bugs = genReport ("html/report_by_"++role++".html") f bugs

byDevReport = byRoleReport "dev" byDev
byQEReport = byRoleReport "qe" byQE


perRoleReport role f bugs = map (\(r,list) -> genReport ("html/report_" ++ role ++ "_" ++ r ++ ".html") byPriority list) tupleList
             where
                nameList = DL.nub $ DL.sort $ map (\b -> f b) bugs
                bugListForEachName = map (filterByName bugs) nameList
                filterByName bugs name = filter (\b -> name == (f b)) bugs
                tupleList = zip nameList bugListForEachName

perRoleSummary role f bugs = genSummaryReport role ("html/report_" ++ role ++ "_summary.html") tupleList
             where
                nameList = DL.nub $ DL.sort $ map (\b -> f b) bugs
                bugCountForEachName = map (length . (filterByName bugs)) nameList
                actionableBugCountForEachName = map (length . filterByStatus . (filterByName bugs)) nameList
                filterByName bugs name = filter (\b -> name == (f b)) bugs
                filterByStatus bugs = filter (\b -> ff (BD.bugStatus b)) bugs
		ff x = if role == "dev" then (x /= "ToTrack" && x /= "ToTest" && x /= "ToBuild") else
					     (x == "ToTest")
                tupleList = DL.sortBy (\(_,_,c1) (_,_,c2)-> compare c2 c1) $ zip3 nameList bugCountForEachName actionableBugCountForEachName
		

perStatusReport bugs =
  let statusList = DL.nub $ DL.sort $ map (\b -> BD.bugStatus b) bugs
      filterByStatus bugs status = filter (\b -> status == (BD.bugStatus b)) bugs
      bugListByStatus = map (filterByStatus bugs) statusList
      tupleList = zip statusList bugListByStatus
  in
      map (\(s,list) -> genReport ("html/report_status_" ++ s ++ ".html") byPriority list) tupleList


perVersionReport bugs =
  let versionList = DL.nub $ DL.sort $ map (\b -> BD.bugVersion b) bugs
      filterByVersion bugs status = filter (\b -> status == (BD.bugVersion b)) bugs
      bugListByVersion = map (filterByVersion bugs) versionList
      tupleList = zip versionList bugListByVersion
  in
      map (\(s,list) -> genReport ("html/report_version_" ++ s ++ ".html") byPriority list) tupleList


perFixByMilestoneReport bugs =
  let milestoneList = DL.nub $ DL.sort $ map (\b -> BD.bugFixByMilestone b) bugs
      filterByMilestone bugs milestone = filter (\b -> milestone == (BD.bugFixByMilestone b)) bugs
      bugListByMilestone = map (filterByMilestone bugs) milestoneList
      tupleList = zip milestoneList bugListByMilestone
  in
      map (\(s,list) -> genReport ("html/report_milestone_" ++ s ++ ".html") byPriority list) tupleList

perDevReport = perRoleReport "dev" BD.bugDev
perQEReport = perRoleReport "qe" BD.bugQE

perDevSummaryReport = perRoleSummary "dev" BD.bugDev
perQESummaryReport = perRoleSummary "qe" BD.bugQE



--mapMI :: (a -> IO b) -> [a] -> IO [b]
--mapMI _ [] = return [] -- You can play with this case a bit. This will open a file for the head of the list,
---- and then when each subsequent cons cell is inspected. You could probably
---- interleave 'f x' as well.
--mapMI f (x:xs) = do y <- SIU.unsafeInterleaveIO (f x) ; ys <- SIU.unsafeInterleaveIO (mapMI f xs) ; return (y:ys)


report :: [BD.Bug] -> IO ()
report bugs = do
       byPriorityReport bugs
       --byDevReport bugs
       --byQEReport bugs
       --byUpdateDateReport bugs
       --perDevSummaryReport bugs
       --perQESummaryReport bugs
       --foldr (>>) (return ()) $ (perDevReport bugs)
       --foldr (>>) (return ()) $ (perQEReport bugs)
       --foldr (>>) (return ()) $ (perStatusReport bugs)
       --foldr (>>) (return ()) $ (perVersionReport bugs)
       --foldr (>>) (return ()) $ (perFixByMilestoneReport bugs)
       
