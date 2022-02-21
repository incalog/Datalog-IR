package inca.treesitterAPI.treesitterAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

@Structure.FieldOrder({"context", "id", "tree", "diff_heap"})
public class TSNode extends Structure {
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