package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"id", "tag", "link", "parent_id", "parent_tag"})
public class Detach extends Structure {

    public Pointer id;
    public short tag;
    public int link;
    public Pointer parent_id;
    public int parent_tag;

    public Detach() {
        super();
    }
    public Detach(Pointer p) {
        super(p);
        this.read();
    }
}
