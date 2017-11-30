class C {
  void a() {}
  void b() {}
  void c() {}
}

public class Main {
  public static void main(String[] args) {
    C foo = new C();
    foo.a();
    go(foo); // should warn
  }
  static void go(C x) {
    x.b();
    goagain(x);
  }
  static void goagain(C y) {
    y.c();
  }
}
