public class T2 {
  void f(Object x, Object y) {
    //assert x != y;
    while (x == y) {}
  }
  void g() {
    f(null, null);
  }
}
