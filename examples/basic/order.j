//  Inspired by
//    org.apache.tools.ant.filters.ConcatFilter
//  You can concatenate files by making a list of streams.

property "out of order read"
  start -> tracking: *.init(A, B)
  tracking -> has:   true := a.hasMore()  // TODO: stay in tracking ...
  has -> error:      b.read()

class Byte {}

class Stream
  Byte read()
  Bool hasMore()

class ConcatStream  // TODO: Should really inherit from Stream
  var Stream first
  var Stream second

  Unit init(Stream a, Stream b)
    first := a
    second := b

  // The property to check is that first.hasMore() returned true
  // before second.read() is called.
  Byte read()
    var Bool f := first.hasMore()
    var Byte r
    if *
      r := first.read()
    else
      r := second.read()
    return r

  Bool hasMore()
    var Bool f := first.hasMore()
    var Bool s := second.hasMore()
    return f || s

main
  var Bool m
  var Stream a := new
  var Stream b := new
  var ConcatStream c := new
  c.init(a, b)
  do { m := c.hasMore() }
  while *
    c.read()
