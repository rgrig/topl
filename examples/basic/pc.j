property "empty" {}

property "only start"
  start -> start: *.m()

property "only error"
  error -> error: *.m()

property "redundant"
  start -> error: *.m()
  redundant -> redundant: *.m()

property "cycle"
  start -> error: *.m()
  error -> start: *.m()

property "foo"
  start -> a: *.m()
  a -> b: *.m()
  b -> c: *.m()
  b -> d: *.m()
  c -> e: *.m()
  d -> e: *.m()
  e -> f: *.m()
  f -> a: *.m()
  c -> bar: *.m()
  foo -> d: *.m()
  f-> error: *.m()

property "non-linear pattern"
  start -> a: X := X.next()
  a -> error: X := Y.foo(X, Y)

property "unset variable"
  start -> error: x.foo()

property "possibly unset variable"
  start -> a: X.foo()
  a -> b:     x.foo()  // this one is OK
  start -> b: *.bar()
  b -> error: x.foo()

// should be OK
property "trying to advance an iterator past the end"
  start -> tracking:    I := *.iterator()
  tracking -> notAtEnd: true := i.hasNext()
  notAtEnd -> tracking: i.next()
  tracking -> error:    i.next()


