package inca.treesitterAPI;

import inca.treesitterAPI.editscriptAPI.EditScript;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"constructed_tree", "edit_script", "success"})
public class TSDiffResult extends Structure {
    public static class ByValue extends TSDiffResult implements Structure.ByValue { }

    public TSTree constructed_tree;
    public EditScript.ByReference edit_script;
    public boolean success;

    public TSDiffResult() {
        super();
    }
    public TSDiffResult(Pointer p) {
        super(p);
        this.read();
    }
}