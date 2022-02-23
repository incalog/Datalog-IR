package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    void *id;
    TSSymbol tag;   TSSymbol: alias for uint16_t.
    uint32_t link;
    void *parent_id;
    TSSymbol parent_tag;
} Detach;
*/

@Structure.FieldOrder({"id", "tag", "link", "parent_id", "parent_tag"})
public class Detach extends Structure {

    public Pointer id;
    public short tag;
    public int link;
    public Pointer parent_id;
    public short parent_tag;

    public Detach() {
        super();
    }

    public Detach(Pointer p) {
        super(p);
        this.read();
    }
}
