class A
  var B ab
  Unit ab(B b) { ab := b }
  Unit aa() { ab.ba(*) }
class B
  var A ba
  Unit ba(A a) { ba := a }
  Unit go()
    ba := new
    ba.ab(this)
    ba.aa()
property "aa() called on null"
  start -> error: * := *.aa()   // TODO: Add keyword 'null'.
main
  var B b := new
  b.go()
