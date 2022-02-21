package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

@Structure.FieldOrder({"is_leaf", "symbol", "id", "edit_data"})
public class Load extends Structure {

    public byte is_leaf; // packs value of is_leaf into this field
    public short symbol;
    public Pointer id;
    public EditData edit_data;

    public static class EditData extends Union {

        public EditLeafData leaf;
        public EditNodeData node;
    }

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
