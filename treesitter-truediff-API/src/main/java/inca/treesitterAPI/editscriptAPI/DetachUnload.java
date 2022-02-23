package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    void *id;
    uint32_t link;
    void *parent_id;
    TSSymbol parent_tag;    TSSymbol: alias for uint16_t.
    TSSymbol tag;
    ChildPrototypeArray kids;
} DetachUnload;
*/

@Structure.FieldOrder({"id", "link", "parent_id", "parent_tag", "tag", "kids"})
public class DetachUnload extends Structure {

    public Pointer id;
    public int link;
    public Pointer parent_id;
    public short parent_tag;
    public short tag;
    public ChildPrototypeArray kids;

    public DetachUnload() {
        super();
    }

    public DetachUnload(Pointer p) {
        super(p);
        this.read();
    }
}
