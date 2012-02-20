property IteratorComodification
  observing <java.util.{Iterator.{remove,next},Collection.iterator}>
  prefix <java.util.{Iterator,Collection}>
  start -> gotOne:    I := C.iterator()
  gotOne -> gotTwo:   J := c.iterator()
  gotOne -> gotOne:   *
  gotTwo -> jInvalid: call i.remove()
  gotTwo -> iInvalid: call j.remove()
  jInvalid -> error:  call j.next()
  iInvalid -> error:  call i.next()
