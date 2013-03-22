module HTMLCommon where

style = "\
 \<style>\
 \ body {\
 \     font-family:arial,helvetica,sans-serif;\
 \     font-size:14px;\
 \ }\
 \.title {\
 \  display:inline;\
 \}\
 \.note {\
 \  border:1px solid #bbb;\
 \  padding: 0.5em 0.2em 0.5em 0.2em;\
 \}\
 \.record {\
 \  border:1px solid #bbb;\
 \  padding: 0.5em 0.2em 0.5em 0.2em;\
 \}\
 \.recordRed {\
 \  border:2px solid #f00;\
 \  padding: 0.5em 0.2em 0.5em 0.2em;\
 \}\
 \.recordBlue {\
 \  border:2px solid #00f;\
 \  padding: 0.5em 0.2em 0.5em 0.2em;\
 \}\
 \.recordGreen {\
 \  border:4px solid #070;\
 \  padding: 0.5em 0.2em 0.5em 0.2em;\
 \}\
 \.addedBy {\
 \  font-style:italic;\
 \}\
 \.recordBugId {\
 \  display:inline;\
 \  float:left;\
 \}\
 \.HighPriorityHighSeverity {\
 \  color:#f00;\
 \  background-color:#ff0;\
 \  display:inline;\
 \  font-weight:bold;\
 \}\
 \.HighPriorityLowSeverity {\
 \  background-color:#ff0;\
 \  display:inline;\
 \  font-weight:bold;\
 \}\
 \.LowPriorityHighSeverity {\
 \  color:#f00;\
 \  font-weight:bold;\
 \  display:inline;\
 \}\
 \.LowPriorityLowSeverity {\
 \  display:inline;\
 \}\
 \</style>"


htmlStart = "<html><head><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\">" ++ style ++ "</head><body>"
htmlEnd = "</body></html>"

encloseInDIV :: String -> String -> String
encloseInDIV className body = "<DIV class=\"" ++ className ++ "\">" ++ body  ++ "</DIV>"


