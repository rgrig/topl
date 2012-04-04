public class Main {
  public static void main(String[] args) {
    new Main().f(1L, 0);
  }
  long f(long x, int y) {
    long z = 0;
    while (y-- > 0) z += x;
    return z;
  }
}
