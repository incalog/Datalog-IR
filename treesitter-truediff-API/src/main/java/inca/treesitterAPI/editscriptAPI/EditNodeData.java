package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"kids", "production_id"})
public class EditNodeData extends Structure {

    public ChildPrototypeArray kids;
    public short production_id;

    public EditNodeData() {
        super();
    }

    public EditNodeData(Pointer p) {
        super(p);
        this.read();
    }
}
