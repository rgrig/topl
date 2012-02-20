property OtherModified
  message "trying to advance an iterator on a collection modified by another iterator"
  prefix <examples.*>
  observing <examples.*>
  start -> gotOne:    I := C.iterator()
  gotOne -> gotTwo:   J := c.iterator()
  gotOne -> gotOne:   *
  gotTwo -> jInvalid: i.remove()
  gotTwo -> iInvalid: j.remove()
  jInvalid -> error:  j.next()
  iInvalid -> error:  i.next()
