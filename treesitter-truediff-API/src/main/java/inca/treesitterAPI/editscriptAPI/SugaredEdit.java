package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*C definition:
* struct SugaredEdit {
    EditTag edit_tag;
    union {
        Attach attach;
        Detach detach;
        Unload unload;
        Load load;
        Update update;
        UpdatePadding update_padding;
        LoadAttach load_attach;
        DetachUnload detach_unload;
    };
};
*/

@Structure.FieldOrder({"edit_tag", "sugar_edit"})
public class SugaredEdit extends Structure {

    // Use ByReference for pointer in struct fields and ByValue for function arguments and return values.
    public static class ByReference extends SugaredEdit implements Structure.ByReference { }
    public static class ByValue extends SugaredEdit implements Structure.ByValue { }

    public int edit_tag;
    public SugarEdit sugar_edit;

    public static class SugarEdit extends Union {

        public Attach attach;
        public Detach detach;
        public Unload unload;
        public Load load;
        public Update update;
        public UpdatePadding update_padding;
        public LoadAttach load_attach;
        public DetachUnload detach_unload;
    }

    // Use custom read() function to set the appropriate type for the union field sugar_edit.
    @Override
    public void read() {
        super.read();
        switch (edit_tag) {
            case EditTag.ATTACH -> sugar_edit.setType(Attach.class);
            case EditTag.DETACH -> sugar_edit.setType(Detach.class);
            case EditTag.UNLOAD -> sugar_edit.setType(Unload.class);
            case EditTag.LOAD -> sugar_edit.setType(Load.class);
            case EditTag.LOAD_ATTACH -> sugar_edit.setType(LoadAttach.class);
            case EditTag.DETACH_UNLOAD -> sugar_edit.setType(DetachUnload.class);
            case EditTag.UPDATE -> sugar_edit.setType(Update.class);
            case EditTag.UPDATE_PADDING -> sugar_edit.setType(UpdatePadding.class);
            default -> {
            }
        }
        sugar_edit.read();
    }

    public SugaredEdit() {
        super();
    }

    public SugaredEdit(Pointer p) {
        super(p);
        this.read();
    }
}
