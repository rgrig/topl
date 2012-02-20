property BadCall
  message "called badMethod()"
  start -> error: *.badMethod()

class Object
  Unit badMethod()

main
  var Object x := new
  x.badMethod()
