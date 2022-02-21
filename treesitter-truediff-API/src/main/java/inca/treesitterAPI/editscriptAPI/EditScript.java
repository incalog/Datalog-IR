package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"edits"})
public class EditScript extends Structure {
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