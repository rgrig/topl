// Java built-ins that are not builtin here
class Int {}
class Object {}

// Some library stubs
class Collection
  Iterator iterator()
  Unit remove(Object o)
  Int size()
class Iterator
  Bool hasNext()
  Object next()

// Figure 3 in "Modular TypeState Checking of Aliased Objects"
main
  var Collection c := new
  var Iterator it := c.iterator()
  var Bool hasNext
  do { hasNext := it.hasNext() }
  while (hasNext && *)
    var Object o := it.next()
    var Iterator it2 := c.iterator()
    do { hasNext:=it2.hasNext() }
    while (hasNext)
      var Object o2 := it2.next()
  hasNext := it.hasNext()
  var Int cSize := c.size()
  if (hasNext && cSize == *)  // * is really 3 in the original
    var Object o := it.next()
    c.remove(o)
    hasNext := it.hasNext() // Illegal
  var Iterator it3 := c.iterator()
