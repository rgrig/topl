import java.util.Random;

class O {
  private O() {}
  public static O get() {
    return new O();
  }
  public void go() {
  }
}

public class Main {
  public static void main(String[] args) {
    O x = O.get();
    O y = O.get();
    y.go();
    x.go();
    x.go();
  }
}
