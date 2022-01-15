package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"negative_buffer", "positive_buffer"})
public class EditScriptBuffer extends Structure {

    public EditArray negative_buffer;
    public EditArray positive_buffer;

    public EditScriptBuffer() {
        super();
    }

    public EditScriptBuffer(Pointer p) {
        super(p);
        this.read();
    }
}
