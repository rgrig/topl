import java.util.Random;

class G { // global
  static private Object tracked = null;
  static private int state = 0;
  static void get_ret(Object x) {
    if (maybe()) tracked = x;
  }
  static void go_ret(Object x) {
  }
  static void go_call(Object x) {
    if (x == tracked) {
      if (state == 0) state = 1;
      else if (state == 1) {while (true);}
    }
  }
  static boolean maybe() {
    return random.nextBoolean();
  }
  static private Random random = new Random();
}

class O {
  private O() {}
  public static O get() {
    return new O();
  }
  public void go() { }
}

public class T3 {
  public static void main(String[] args) {
    O x = O.get();
    G.get_ret(x);
    O y = O.get();
    G.get_ret(y);
    G.go_call(y);
    y.go();
    G.go_call(x);
    x.go();
    G.go_call(x);
    x.go();
  }
}
