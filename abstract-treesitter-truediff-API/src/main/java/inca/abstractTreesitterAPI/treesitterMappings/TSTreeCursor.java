package inca.abstractTreesitterAPI.treesitterMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
  const void *tree;
  const void *id;
  uint32_t context[2];
} TSTreeCursor;
*/

@Structure.FieldOrder({"tree", "id", "context"})
public class TSTreeCursor extends Structure {

    // Use ByValue for function arguments and return values and ByReference for pointer in struct fields.
    public static class ByValue extends TSTreeCursor implements Structure.ByValue { }
    public static class ByReference extends TSTreeCursor implements Structure.ByReference { }

    public Pointer tree;
    public Pointer id;
    public final int[] context = new int[2];

    public TSTreeCursor() {
        super();
    }

    public TSTreeCursor(Pointer p) {
        super(p);
        this.read();
    }
}