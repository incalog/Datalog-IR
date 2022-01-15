package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"child_id"})
public class ChildPrototype extends Structure {
    public static class ByReference extends ChildPrototype implements Structure.ByReference { }

    public Pointer child_id;

    public ChildPrototype() {
        super();
    }
    public ChildPrototype(Pointer p) {
        super(p);
        this.read();
    }
}
