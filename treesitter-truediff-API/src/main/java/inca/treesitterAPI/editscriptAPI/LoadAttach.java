package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*
* C definition:
* typedef struct {
    bool is_leaf: 1;        Bit Packing. Use type close to original type size to map packed values.
    void *id;
    uint32_t link;
    void *parent_id;
    TSSymbol parent_tag;    TSSymbol: alias for uint16_t.
    TSSymbol tag;
    union {
        EditLeafData leaf;
        EditNodeData node;
    };
} LoadAttach;
*/

@Structure.FieldOrder({"is_leaf", "id", "link", "parent_id", "parent_tag", "tag", "edit_data"})
public class LoadAttach extends Structure {

    public byte is_leaf; // Packs value of is_leaf into this field.
    public Pointer id;
    public int link;
    public Pointer parent_id;
    public short parent_tag;
    public short tag;
    public EditData edit_data;

    public static class EditData extends Union {

        public EditLeafData leaf;
        public EditNodeData node;
    }

    // Use custom read() function to set appropriate type for union field edit_data.
    @Override
    public void read() {
        super.read();
        switch (is_leaf) {
            case 0 -> edit_data.setType(EditNodeData.class);
            case 1 -> edit_data.setType(EditLeafData.class);
            default -> {
            }
        }
        edit_data.read();
    }

    public LoadAttach() {
        super();
    }

    public LoadAttach(Pointer p) {
        super(p);
        this.read();
    }
}
