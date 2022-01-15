package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

@Structure.FieldOrder({"is_leaf", "symbol", "id", "edit_data"})
public class Load extends Structure {

    public short is_leaf;
    public short symbol;
    public Pointer id;
    public EditData edit_data;

    public short getIsLeaf() {
        return (short)(is_leaf & 0x1);
    }

    public static class EditData extends Union {

        public EditLeafData leaf;
        public EditNodeData node;
    }

    public Load() {
        super();
    }
    public Load(Pointer p) {
        super(p);
        this.read();
    }

    @Override
    public void read() {
        super.read();
        if (is_leaf == 0) {
            edit_data.setType(EditNodeData.class);
        } else {
            edit_data.setType(EditLeafData.class);
        }
        edit_data.read();
    }
}
