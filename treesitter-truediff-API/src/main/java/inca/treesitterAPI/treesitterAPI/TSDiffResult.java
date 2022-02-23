package inca.treesitterAPI.treesitterAPI;

import inca.treesitterAPI.editscriptAPI.EditScript;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
    TSTree *constructed_tree;
    EditScript *edit_script;
    bool success;
} TSDiffResult;
*/

@Structure.FieldOrder({"constructed_tree", "edit_script", "success"})
public class TSDiffResult extends Structure {

    // Use ByValue for function arguments and return values.
    public static class ByValue extends TSDiffResult implements Structure.ByValue { }

    public TSTree constructed_tree;
    public EditScript.ByReference edit_script;
    public byte success;

    public TSDiffResult() {
        super();
    }

    public TSDiffResult(Pointer p) {
        super(p);
        this.read();
    }
}