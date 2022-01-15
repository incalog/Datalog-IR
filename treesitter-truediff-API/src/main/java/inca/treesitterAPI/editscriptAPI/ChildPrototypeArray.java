package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"content", "size", "capacity"})
public class ChildPrototypeArray extends Structure {

    public ChildPrototype.ByReference content;
    public int size;
    public int capacity;

    public ChildPrototypeArray() {
        super();
    }
    public ChildPrototypeArray(Pointer p) {
        super(p);
        this.read();
    }
}
