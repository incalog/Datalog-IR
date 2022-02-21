package inca.treesitterAPI.treesitterAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"row", "column"})
public class TSPoint extends Structure {
    public int row;
    public int column;

    public TSPoint() {
        super();
    }

    public TSPoint(Pointer p) {
        super(p);
        this.read();
    }
}
