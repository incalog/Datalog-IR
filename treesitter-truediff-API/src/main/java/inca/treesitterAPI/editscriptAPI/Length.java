package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import inca.treesitterAPI.TSPoint;

@Structure.FieldOrder({"bytes", "extent"})
public class Length extends Structure {
    public int bytes;
    public TSPoint extent;

    public Length() {
        super();
    }

    public Length(Pointer p) {
        super(p);
        this.read();
    }
}
