package inca.treesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*C definition:
* typedef struct {
    bool is_leaf: 1;    Bit Packing. Use type close to original type size to map packed values.
    TSSymbol tag;       TSSymbol: alias for uint16_t.
    void *id;
    union {
        EditLeafData leaf;
        EditNodeData node;
    };
} Load;
*/

@Structure.FieldOrder({"is_leaf", "tag", "id", "edit_data"})
public class Load extends Structure {

    public byte is_leaf; // packs value of is_leaf into this field.
    public short tag;
    public Pointer id;
    public EditData edit_data;

    public static class EditData extends Union {

        public EditLeafData leaf;
        public EditNodeData node;
    }

    // Use custom read() function to set the appropriate type for union field edit_data.
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

    public Load() {
        super();
    }

    public Load(Pointer p) {
        super(p);
        this.read();
    }
}
