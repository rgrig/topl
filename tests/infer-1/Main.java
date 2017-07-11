import java.util.Random;

class Bad {
  public Bad(Object a, Object b, Object c) {}
  static void s4(Object a, Object b, Object c, Object d) {}
  void i4(Object a, Object b, Object c, Object d) {}
}

public class Main {
  public static void main(String[] args) {
    Bad.s4(null, null, null, null);
    Bad x = new Bad(null, null, null);
    x.i4(null, null, null, null);
  }
}
