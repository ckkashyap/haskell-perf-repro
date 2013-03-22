module BugXML2HS where

import qualified BugData as B
import RSXP as X
import qualified Data.Time as DT
import qualified System.Locale as SL


str2date :: String -> DT.LocalTime
str2date str  = DT.readTime SL.defaultTimeLocale "%Y-%m-%d %H:%M:%S.0" str

getTagValue :: String -> X.XMLAST -> Maybe String
getTagValue name xml = let v = getBodiesByName name xml
                       in case v of
                          [s] -> Just s
                          _   -> Nothing



getBugGroupBugs :: X.XMLAST -> [B.BugGroupBug]
getBugGroupBugs xml =
  let bugGroupBugId    = map ext $ getElementsByPath "/bug/bugGroups/bugGroup/bug/id" xml
      bugGroupProduct  = map ext $ getElementsByPath "/bug/bugGroups/bugGroup/bug/product" xml
      bugGroupPriority  = map ext $ getElementsByPath "/bug/bugGroups/bugGroup/bug/priority" xml
      bugGroupSeverity  = map ext $ getElementsByPath "/bug/bugGroups/bugGroup/bug/severity" xml
      bugGroupStatus  = map ext $ getElementsByPath "/bug/bugGroups/bugGroup/bug/status" xml
      ext (_, _, Body str) = str
      ext _ = "***** SOMETHING TERRIBLE HAS HAPPENED *****"
      specialZip [] [] [] [] [] = []
      specialZip (x1:x1s) (x2:x2s) (x3:x3s) (x4:x4s) (x5:x5s) = (x1,x2,x3,x4,x5) : specialZip x1s x2s x3s x4s x5s
      specialZip _ _ _ _ _ = []
      mapper (i,pd,pr,sev,stat) = B.BugGroupBug {
                              B.bugGroupBugId   = read i :: Int
                            , B.bugGroupProduct           = pd
                            , B.bugGroupPriority          = read pr :: Int
                            , B.bugGroupSeverity          = read sev :: Int
                            , B.bugGroupStatus          = stat
                           }
  in
    map mapper ((specialZip bugGroupBugId bugGroupProduct bugGroupPriority bugGroupSeverity bugGroupStatus))

getBugNotes :: X.XMLAST -> [B.BugNote]
getBugNotes xml = 
  let notes    = map ext $ getElementsByPath "/bug/bugNotes/bugNote/note" xml
      summary  = map ext $ getElementsByPath "/bug/bugNotes/bugNote/summary" xml
      dates    = map ext $ getElementsByPath "/bug/bugNotes/bugNote/dateAdded" xml
      addedBys = map ext $ getElementsByPath "/bug/bugNotes/bugNote/addedBy" xml
      seqNos   = map ext $ getElementsByPath "/bug/bugNotes/bugNote/sequenceNumber" xml
      ext (_, _, Body str) = str
      ext _ = "***** SOMETHING TERRIBLE HAS HAPPENED *****"
      specialZip [] [] [] [] [] = []
      specialZip (x1:x1s) ("A":x2s) (x3:x3s) (x4:x4s) (x5:x5s) = specialZip x1s x2s x3s x4s x5s
      specialZip (x1:x1s) (x2:x2s) (x3:x3s) (x4:x4s) (x5:x5s) = (x1,x2,x3,x4,x5) : specialZip x1s x2s x3s x4s x5s
      specialZip _ _ _ _ _ = []
      mapper (n,_,d,a,s) = B.BugNote {
                              B.noteSequenceNumber   = read s :: Int
                            , B.noteString           = n
                            , B.noteAddedBy          = a
                            , B.noteDate             = str2date d
                           }
  in
    map mapper ((specialZip notes summary dates addedBys seqNos))
                   
getBugPlatforms :: X.XMLAST -> [B.BugPlatform]
getBugPlatforms xml =
  let os = map ext $ getElementsByPath "/bug/platforms/bugPlatform/os" xml
      language = map ext $ getElementsByPath "/bug/platforms/bugPlatform/language" xml
      status = map ext $ getElementsByPath "/bug/platforms/bugPlatform/testStatus" xml
      ext (_, _, Body str) = str
      ext _ = "***** SOMETHING TERRIBLE HAS HAPPENED *****"
      specialZip [] [] [] = []
      specialZip (x1:x1s) (x2:x2s) (x3:x3s) = (x1,x2,x3) : specialZip x1s x2s x3s
      specialZip _ _ _ = []
      mapper (o, l , s) = B.BugPlatform  {
                                      B.os = o
                                    , B.language = l
                                    , B.testStatus = s

                          }
  in map mapper (specialZip os language status)
  

getBugAttachments :: X.XMLAST -> [B.BugAttachment]
getBugAttachments xml =
  let systemFileName       = map ext $ getElementsByPath "/bug/bugAttachments/bugAttachment/systemFilename" xml
      userFileName         = map ext $ getElementsByPath "/bug/bugAttachments/bugAttachment/userFilename" xml
      fileLocation             = map ext $ getElementsByPath "/bug/bugAttachments/bugAttachment/fileLocation" xml
      ext (_, _, Body str) = str
      ext _                = "***** SOMETHING TERRIBLE HAS HAPPENED *****"
      specialZip [] [] [] = []
      specialZip (x1:x1s) (x2:x2s) (x3:x3s) = (x1,x2,x3) : specialZip x1s x2s x3s
      specialZip _ _ _ = []
      mapper (s, u , p) = B.BugAttachment  {
                                      B.systemFileName = s
                                    , B.userFileName = u
                                    , B.fileLocation = p

                          }
  in map mapper (specialZip systemFileName userFileName fileLocation)
  


parseBug :: X.XMLAST -> Maybe B.Bug
parseBug es@(X.Element _ _ e) = do
         bugId'                <- getTagValue "/bug/id" es
         bugFamily'            <- getTagValue "/bug/family" es
         bugProduct'           <- getTagValue "/bug/product" es
         bugVersion'           <- getTagValue "/bug/version" es
         bugState'             <- getTagValue "/bug/state" es
         bugStatus'            <- getTagValue "/bug/status" es
         bugReason'            <- getTagValue "/bug/reason" es
         bugSeverity'          <- getTagValue "/bug/severity" es
         bugPriority'          <- getTagValue "/bug/priority" es
         bugProductArea'       <- getTagValue "/bug/productArea" es
         bugSubArea'           <- getTagValue "/bug/subArea" es
         bugOwner'             <- getTagValue "/bug/owner" es
         bugDev'               <- getTagValue "/bug/dev" es
         bugQE'                <- getTagValue "/bug/qe" es
         bugTitle'             <- getTagValue "/bug/title" es
         bugDescription'       <- getTagValue "/bug/description" es
         bugFixByMilestone'    <- getTagValue "/bug/fixByMilestone" es
         bugGroupRelationship' <- getTagValue "/bug/bugGroupRelationship" es
         bugGroupId'           <- getTagValue "/bug/bugGroupId" es
         bugCreatedBy'         <- getTagValue "/bug/createdBy" es
         bugDateCreated'       <- getTagValue "/bug/dateCreated" es
         bugDateUpdated'       <- getTagValue "/bug/dateUpdated" es
         
         return (B.Bug {
                B.bugId                = read bugId'
              , B.bugFamily            = bugFamily'
              , B.bugProduct           = bugProduct'
              , B.bugVersion           = bugVersion'
              , B.bugState             = bugState'
              , B.bugStatus            = bugStatus'
              , B.bugReason            = bugReason'
              , B.bugSeverity          = read bugSeverity'
              , B.bugPriority          = read bugPriority'
              , B.bugProductArea       = bugProductArea'
              , B.bugSubArea           = bugSubArea'
              , B.bugOwner             = bugOwner'
              , B.bugDev               = bugDev'
              , B.bugQE                = bugQE'
              , B.bugTitle             = bugTitle'
              , B.bugDescription       = bugDescription'
              , B.bugFixByMilestone    = bugFixByMilestone'
              , B.bugGroupRelationship = bugGroupRelationship'
              , B.bugGroupId           = bugGroupId'
              , B.bugCreatedBy         = bugCreatedBy'
              , B.bugDateCreated       = str2date bugDateCreated'
              , B.bugDateUpdated       = str2date bugDateUpdated'
              , B.bugNotes             = getBugNotes es
              , B.bugPlatforms         = getBugPlatforms es
              , B.bugAttachments       = getBugAttachments es
              , B.bugGroupBugs         = getBugGroupBugs es
                })
parseBug _ = Just B.badBug






