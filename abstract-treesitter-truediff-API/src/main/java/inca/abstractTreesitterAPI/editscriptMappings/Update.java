package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    void *id;
    TSSymbol tag;           TSSymbol: alias for uint16_t.
    Length old_start;
    Length old_size;
    Length new_start;
    Length new_size;
} Update;
*/

@Structure.FieldOrder({"id", "tag", "old_start", "old_size", "new_start", "new_size"})
public class Update extends Structure {

    public Pointer id;
    public short tag;
    public Length old_start;
    public Length old_size;
    public Length new_start;
    public Length new_size;

    public Update() {
        super();
    }

    public Update(Pointer p) {
        super(p);
        this.read();
    }
}
