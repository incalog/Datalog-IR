package inca.treesitterAPI.treesitterAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"tree", "id", "context"})
public class TSTreeCursor extends Structure {
    public static class ByValue extends TSTreeCursor implements Structure.ByValue {
        Structure.ByReference asReference() {
            return new TSTreeCursor.ByReference(this.getPointer());
        }
    }

    public static class ByReference extends TSTreeCursor implements Structure.ByReference {
        public ByReference(Pointer p) {
            super(p);
        }
    }

    public TSTreeCursor() {
        super();
    }

    public TSTreeCursor(Pointer p) {
        super(p);
        this.read();
    }

    public Pointer tree;
    public Pointer id;
    public final int[] context = new int[2];
}