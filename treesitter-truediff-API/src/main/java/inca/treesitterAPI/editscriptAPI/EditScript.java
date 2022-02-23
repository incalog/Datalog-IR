package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* struct EditScript {
    EditArray edits;
};
*/

@Structure.FieldOrder({"edits"})
public class EditScript extends Structure {

    // Use ByReference for pointer in struct fields.
    public static class ByReference extends EditScript implements Structure.ByReference { }

    public EditArray edits;

    public EditScript() {
        super();
    }

    public EditScript(Pointer p) {
        super(p);
        this.read();
    }
}