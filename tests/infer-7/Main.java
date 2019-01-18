import topl.Property;

class C {
  void a() {}
  void b() {}
  void c() {}
}

public class Main {
  public static void main(String[] args) {
    C foo = new C();
    if (foo == topl.Property.r0) {} else {}
    foo.a();
    go(foo); // should warn
  }
  static void go(C x) {
    if (x == topl.Property.r0) {} else {}
    x.b();
    goagain(x);
  }
  static void goagain(C y) {
    if (y == topl.Property.r0) {} else {}
    y.c();
  }
}
