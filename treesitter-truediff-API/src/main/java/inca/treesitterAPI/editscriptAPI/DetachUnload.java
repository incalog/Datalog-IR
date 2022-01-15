package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

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
