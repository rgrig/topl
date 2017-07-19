class T {
  static T create() { return new T(); }
  static T chain(T x, T y) { return new T(); }
  void bad() {}
}

public class Main {
  public static void main(String[] args) {
    T x = T.create();
//    f1(x);
//    f2(x);
    f3(x);
  }

  // Should warn. Uses nonunit transitions.
  static void f1(T x) {
    x = T.chain(x, x);
    x.bad();
  }

  // Should warn. Uses only unit transitions.
  static void f2(T x) {
    x.bad();
  }

  // Should not warn. (TODO)
  static void f3(T x) {
    x = new T();
    x.bad();
  }
}
