package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

@Structure.FieldOrder({"is_leaf", "id", "link", "parent_id", "parent_tag", "tag", "edit_data"})
public class LoadAttach extends Structure {

    public byte is_leaf;
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

    public LoadAttach() {
        super();
    }

    public LoadAttach(Pointer p) {
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
