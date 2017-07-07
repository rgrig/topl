//class E extends Exception {}

public class Main {
  public static void main(String[] args) {
    new Main().go();
//    try { throw new E(); }
//    catch (Exception e) { System.out.println("caught"); }
  }

  void go() {
    synchronized(this) {}
  }
}
