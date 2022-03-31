package inca.abstractTreesitterAPI.treesitterMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct {
  uint32_t context[4];
  const void *id;
  const TSTree *tree;
  const TSDiffHeap *diff_heap;
} TSNode;
*/

@Structure.FieldOrder({"context", "id", "tree", "diff_heap"})
public class TSNode extends Structure {

    // Use ByValue for function arguments and return values.
    public static class ByValue extends TSNode implements Structure.ByValue { }

    public final int[] context = new int[4];
    public Pointer id;
    public TSTree tree;
    public TSDiffHeap diff_heap;

    public TSNode() {
        super();
    }

    public TSNode(Pointer p) {
        super(p);
        this.read();
    }
}