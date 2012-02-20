//  This is inspired by 
//    org.apache.tools.ant.ComponentHelper.getRestrictedDefinitions
//  It returns a List and says that it is "live" and should not be modified.

class Object {}

class List
  Bool add(Object o)

class Foo
  List bar()
    var List r := new
    return r

main
  var Foo foo := new
  var List l := foo.bar()
  var Object o := new
  l.add(o)
