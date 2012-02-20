property Null
  message "go() called on null"
  prefix <examples.*>
  observing <examples.*>
  start -> start: return *.go()
  start -> error: call <null>.go()

