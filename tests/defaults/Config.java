import static topl.Property.checker;
public class Config {
  public static void main(String[] args) {
    checker.captureCallStacks = false;
    Main.main(args);
  }
}
