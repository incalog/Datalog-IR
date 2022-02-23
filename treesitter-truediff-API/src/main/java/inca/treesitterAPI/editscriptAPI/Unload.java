package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    TSSymbol tag;                   TSSymbol: alias for uint16_t.
    void *id;
    ChildPrototypeArray kids;
} Unload;
*/

@Structure.FieldOrder({"tag", "id", "kids"})
public class Unload extends Structure {

    public short tag;
    public Pointer id;
    public ChildPrototypeArray kids;

    public Unload() {
        super();
    }

    public Unload(Pointer p) {
        super(p);
        this.read();
    }
}
