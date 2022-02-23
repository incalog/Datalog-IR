package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    void *id;
    TSSymbol tag;   TSymbol: alias for uint16_t.
    uint32_t link;
    void *parent_id;
    TSSymbol parent_tag;
} Attach;
*/

@Structure.FieldOrder({"id", "tag", "link", "parent_id", "parent_tag"})
public class Attach extends Structure {

    public Pointer id;
    public short tag;
    public int link;
    public Pointer parent_id;
    public int parent_tag;

    public Attach() {
        super();
    }

    public Attach(Pointer p) {
        super(p);
        this.read();
    }
}
