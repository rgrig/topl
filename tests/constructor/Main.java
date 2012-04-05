class A {
  void f() {}
}

public class Main {
  public static void main(String[] args) {
    new Main().f();
  }
  void f() {
    A x = new A();
  }
}
