module BugData (
         Bug(Bug)
       , bugId
       , bugFamily
       , bugProduct
       , bugVersion
       , bugState
       , bugStatus
       , bugReason
       , bugSeverity
       , bugPriority
       , bugProductArea
       , bugSubArea
       , bugOwner
       , bugDev
       , bugQE
       , bugTitle
       , bugDescription
       , bugFixByMilestone
       , bugGroupRelationship
       , bugGroupId
       , bugCreatedBy
       , bugDateCreated
       , bugDateUpdated
       , bugNotes
       , bugPlatforms
       , bugAttachments
       , bugGroupBugs

       , BugNote (BugNote)
       , noteSequenceNumber
       , noteString        
       , noteAddedBy       
       , noteDate

       , BugPlatform (BugPlatform)
       , os
       , language
       , testStatus

       , BugAttachment (BugAttachment)
       , systemFileName
       , userFileName  
       , fileLocation

       , BugGroupBug (BugGroupBug)
       , bugGroupBugId
       , bugGroupProduct
       , bugGroupSeverity 
       , bugGroupPriority 
       , bugGroupStatus   

        

       , badBug
      


   ) where
import qualified Data.Time as DT
import qualified System.Locale as SL
import Control.DeepSeq

data Bug = Bug {
   bugId                :: Int
 , bugFamily            :: String
 , bugProduct           :: String
 , bugVersion           :: String
 , bugState             :: String
 , bugStatus            :: String
 , bugReason            :: String
 , bugSeverity          :: Int
 , bugPriority          :: Int
 , bugProductArea       :: String
 , bugSubArea           :: String
 , bugOwner             :: String
 , bugDev               :: String
 , bugQE                :: String
 , bugTitle             :: String
 , bugDescription       :: String
 , bugFixByMilestone    :: String
 , bugGroupRelationship :: String
 , bugGroupId           :: String
 , bugCreatedBy         :: String
 , bugDateCreated       :: DT.LocalTime
 , bugDateUpdated       :: DT.LocalTime
 , bugNotes             :: [BugNote]
 , bugPlatforms         :: [BugPlatform]
 , bugAttachments       :: [BugAttachment]
 , bugGroupBugs         :: [BugGroupBug]
         
     } deriving (Show)
     
data BugNote = BugNote {
   noteSequenceNumber   :: Int
 , noteString           :: String
 , noteAddedBy          :: String
 , noteDate             :: DT.LocalTime
     } deriving (Show)

data BugPlatform = BugPlatform {
   os                   :: String
 , language             :: String
 , testStatus           :: String
     } deriving Show


data BugAttachment = BugAttachment {
   systemFileName       :: String
 , userFileName         :: String
 , fileLocation         :: String
     } deriving Show

data BugGroupBug = BugGroupBug {
   bugGroupBugId       :: Int
 , bugGroupProduct  :: String
 , bugGroupSeverity :: Int
 , bugGroupPriority :: Int
 , bugGroupStatus   :: String
 
     } deriving Show


badBug = Bug {
   bugId                = 0
 , bugFamily            = "THIS IS A DUMMY BUG - KASHYAP, THE REPORTING PROGRAM IS MOST LIKELY NOT WORKING"
 , bugProduct           = ""
 , bugVersion           = ""
 , bugState             = ""
 , bugStatus            = ""
 , bugReason            = ""
 , bugSeverity          = 0
 , bugPriority          = 0
 , bugProductArea       = ""
 , bugSubArea           = ""
 , bugOwner             = ""
 , bugDev               = ""
 , bugQE                = ""
 , bugTitle             = ""
 , bugDescription       = ""
 , bugFixByMilestone    = ""
 , bugGroupRelationship = ""
 , bugGroupId           = ""
 , bugCreatedBy         = ""
 , bugDateCreated       = DT.readTime SL.defaultTimeLocale "%Y-%m-%d %H:%M:%S.0" "1111-11-11 11:11:11.0"
 , bugDateUpdated       = DT.readTime SL.defaultTimeLocale "%Y-%m-%d %H:%M:%S.0" "1111-11-11 11:11:11.0"
 , bugNotes             = []
 , bugPlatforms         = []
 , bugAttachments       = []
 , bugGroupBugs         = []
 }
