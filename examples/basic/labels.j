property Labels
  message "Nonsense, but covering lots of labels"
  observing <java.util.{Iterator,Collection}.*>
  // observing names
  // observing relative
  // observing specific
  prefix <java.util.Iterator>
  prefix <java.util.Collection>
  a -> b: x.m(y, z)
  a -> b: x.m(*, z)
  a -> b: *.m(y, z)
  a -> b: x.m[2]
  a -> b: x.m[*]
  a -> b: x.*(y, z)
  a -> b: x.*(*, z)
  a -> b: x.*[2]
  a -> b: x.*
  a -> b: *.*
  a -> b: *
  a -> b: w := x.m(y, z)
  a -> b: w := x.m(*, z)
  a -> b: w := *.m(y, z)
  a -> b: w := x.m[2]
  a -> b: w := x.m[*]
  a -> b: w := x.*(y, z)
  a -> b: w := x.*(*, z)
  a -> b: w := x.*[2]
  a -> b: w := x.*
  a -> b: w := *.*
  a -> b: w := *
  a -> b: * := x.m(y, z)
  a -> b: * := x.m(*, z)
  a -> b: * := *.m(y, z)
  a -> b: * := x.m[2]
  a -> b: * := x.m[*]
  a -> b: * := x.*(y, z)
  a -> b: * := x.*(*, z)
  a -> b: * := x.*[2]
  a -> b: * := x.*
  a -> b: * := *.*
  a -> b: * := *
  a -> b: call x.m(y, z)
  a -> b: call x.m(*, z)
  a -> b: call *.m(y, z)
  a -> b: call x.m[2]
  a -> b: call x.m[*]
  a -> b: call x.*(y, z)
  a -> b: call x.*(*, z)
  a -> b: call x.*[2]
  a -> b: call x.*
  a -> b: call *.*
  a -> b: call *
  a -> b: return w := *.m(*, *)
  a -> b: return w := *.m[2]
  a -> b: return w := *.m[*]
  a -> b: return w := *.*[2]
  a -> b: return w := *.*
  a -> b: return w := *
  a -> b: return * := *.m(*, *)
  a -> b: return * := *.m[2]
  a -> b: return * := *.m[*]
  a -> b: return * := *.*[2]
  a -> b: return * := *.*
  a -> b: return * := *
  a -> b: return *.m(*, *)
  a -> b: return *.m[2]
  a -> b: return *.m[*]
  a -> b: return *.*[2]
  a -> b: return *.*
  a -> b: return *
  a -> b: return *.<return>[*]
