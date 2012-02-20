property OtherModified
  message "trying to advance an iterator on a collection modified by another iterator"
  start -> gotOne:    I := C.iterator()
  gotOne -> gotTwo:   J := c.iterator()
  gotTwo -> jInvalid: i.remove()
  gotTwo -> iInvalid: j.remove()
  jInvalid -> error:  j.next()
  iInvalid -> error:  i.next()

property AdvanceModified
  message "trying to advance an iterator on a modified collection"
  start -> tracking:    I := C.iterator()
  tracking -> modified: c.add(*), c.remove(*)
  modified -> error:    i.next()

property AdvanceBlind
  message "advancing iterator without checking if not last"
  start -> tracking:    I := *.iterator()
  tracking -> notAtEnd: <true> := i.hasNext()
  notAtEnd -> tracking: i.next()
  tracking -> error:    i.next()

property RemoveStart
  message "trying to remove before iterator has been advanced"
  start -> created: I := *.iterator()
  created -> ok:    i.next()
  created -> error: i.remove()

property AdvanceEnd
  message "trying to advance an iterator past the end"
  start -> tracking:  I := *.iterator()
  tracking -> atEnd:  <false> := i.hasNext()
  atEnd -> error:     i.next()

class Object {}

class Iterator
  Bool hasNext()
  Object next()
  Unit remove()

class Collection
  Iterator iterator()
  Unit add(Object o)

var Unit unit

class User
  Unit remove(Collection c, Object x)
    var Bool hasNext
    var Iterator i := c.iterator()
    do { hasNext := i.hasNext() }
    while hasNext
      var Object y := i.next()
      if x == y { i.remove() }
    return unit

main
  unit := new
  var User u := new
  var Collection c := new
  var Object o := new
  c.add(o)
  u.remove(c, o)
