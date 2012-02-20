property "wrong arity"
  start -> error: *.m(*,*)

class A
  Unit m(Unit x) {}

main
  var A a := *
  var Unit u := *
  a.m(u)
