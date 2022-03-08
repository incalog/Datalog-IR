package inca.treesitterAPI.treesitterMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
  uint32_t row;
  uint32_t column;
} TSPoint;
*/

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
