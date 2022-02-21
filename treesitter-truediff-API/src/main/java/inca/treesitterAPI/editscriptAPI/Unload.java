package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

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
