// Bierhoff and Aldrich annotate and check the correctness of this
// implementation. Our whole approach is focused on checking how an API
// is used, rather than on how it is implemented. Of course, each API
// is implemented in lower-level APIs, so in some cases we might help
// a bit with correctness checking. For example, in the code below we
// might try to check that ByteArray is used such that get(index) is
// always preceded by set(index, _).

// NOTE: There is one more example in the Modular Typestates paper that
// is about checking an implementation in the presence of inheritance.

// Some stubs, for things that are not built in the language.
class Byte {}

class ByteArray
  Unit init(Int size)
  Unit set(Int index, Byte data)
  Byte get(Int index)
  var Int length

class Int
  Unit increment()
  Unit fromByte(Byte b)
  Bool lt(Int x)
  Bool le(Int x)
  Byte toByte()

// Figure 7 in "Modular TypeState Checking of Aliased Objects"
class PipedOutputStream
  var PipedInputStream sink

  Unit connect(PipedInputStream snk)
    sink := snk

  Unit write(Int b)
    sink.receive(b)

  Unit close()
    sink.receivedLast()

// Figure 8 in "Modular TypeState Checking of Aliased Objects"
class PipedInputStream
  var Bool closedByWriter
  var Bool closedByReader
  var ByteArray buffer
  var Int in
  var Int out

  Unit init(PipedOutputStream src)
    closedByWriter := * // * was 0
    closedByReader := * // * was 0
    buffer := new
    buffer.init(*)      // * was 1024
    in := *             // * was -1
    out := *            // * was 0
    src.connect(this)

  Unit receive(Int b)
    var Bool c  // condition
    while (in == out)
      in := * // to break deadlock
    c := in.lt(*) // was 0
    if c
      in := * // was 0
      out := * // was 0
    in.increment()
    var Byte bb := b.toByte()
    buffer.set(in, bb)
    var Int len := buffer.length
    c := len.le(in)
    if c { in := * } // was 0
 
  Unit receivedLast() { closedByWriter := * } // was 1

  Int read()
    var Bool c
    do { c:=in.lt(*)} // * should be 0
    while (in == out || c)
      in := *
    var Byte r := buffer.get(in)
    var Int rr := new
    rr.fromByte(r)
    in.increment()
    return rr

  Unit close()
    closedByReader := * // was 1
    in := * // was -1

main
  var PipedInputStream i := new
  var PipedOutputStream o := new
  var Int x
  i.init(o)
  o.connect(i)
  o.write(x)
  o.close()
  x := i.read()
  i.close()
