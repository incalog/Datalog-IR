package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import inca.abstractTreesitterAPI.treesitterMappings.TSPoint;

/*
* C definition:
* typedef struct {
  uint32_t bytes;
  TSPoint extent;
} Length;
*/

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
