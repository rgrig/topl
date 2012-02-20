package topl;

import java.io.FileWriter;
import static topl.Checker.*;

/** Used for debugging. */
public class PropertyToDOT {
    public static void main(String[] args) throws Exception {
	String s = Property.checker.toDOT(10);
        FileWriter f = new FileWriter("Property.dot");
        f.write(s);
        f.close();
    }
}
// vim:sts=4:sw=4:
