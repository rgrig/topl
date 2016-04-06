class XArray {
  int[] data;
  private XArray() {}
  static XArray allocate(int size) {
    XArray r = new XArray();
    r.data = new int[size];
    return r;
  }
  public void set(int index, int value) { data[index] = value; }
  public int get(int index) { return data[index]; }
}

public class Main {
  public static void main(String[] args) {
    XArray xs = XArray.allocate(10);
    xs.set(3, 2);
    System.out.printf("%d\n", xs.get(3));
  }
}
