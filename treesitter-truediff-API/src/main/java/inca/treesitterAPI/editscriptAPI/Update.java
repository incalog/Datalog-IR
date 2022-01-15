package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

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
