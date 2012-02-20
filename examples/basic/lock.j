// A typical use of locks: One should check that certain methods are
// called only when certain locks are held.

class Lock
  Unit lock()
  Unit unlock()

class A
  var Lock l
  var Bool a // guarded by l
  var B b // has a field guarded by l

  Unit seta(Bool newA)
    a := newA
  Unit setb(Bool newB)
    b.set(newB)
  Unit init()
    l := new
    b := new

  // Forwarding, like writing to fields, is explicit so that 
  // (possible) side-effects are easier to spot.
  Unit lock()
    l.lock()
  Unit unlock()
    l.unlock()

class B
  var Bool b;  guarded by (all) l ('s of As that point to this thru A.b)

  Unit set(Bool newB)
    b := newB

main
  var A a := new
  a.init()
  a.lock()
  a.seta(*)  // OK
  a.unlock()
  a.setb(*) // NOK
  return a.b.b // OK
    // NOTE: If we want to enforce holding the lock on reading then
    // reading too must be wrapped in a method call. (Writing *must* be
    // wrapped with the current version of the language.)
