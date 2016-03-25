import topl.Property;
public class Config {
  public static void main(String[] args) {
    Property.start();
    Main.main(args);
    Property.stop();
  }
}
