package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    void *child_id;
} ChildPrototype;
*/

@Structure.FieldOrder({"child_id"})
public class ChildPrototype extends Structure {

    // Use ByReference for pointer in struct fields.
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
