package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.PointerByReference;

@Structure.FieldOrder({"content", "size", "capacity"})
public class EditArray extends Structure {

    public SugaredEdit.ByReference content;
    public int size;
    public int capacity;

    public EditArray() {
        super();
    }
    public EditArray(Pointer p) {
        super(p);
        this.read();
    }
}
