package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

@Structure.FieldOrder({"edit_tag", "sugar_edit"})
public class SugaredEdit extends Structure {
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

    @Override
    public void read() {
        super.read();
        switch (edit_tag) {
            case 0 -> sugar_edit.setType(Attach.class);
            case 1 -> sugar_edit.setType(Detach.class);
            case 2 -> sugar_edit.setType(Unload.class);
            case 3 -> sugar_edit.setType(Load.class);
            case 4 -> sugar_edit.setType(LoadAttach.class);
            case 5 -> sugar_edit.setType(DetachUnload.class);
            case 6 -> sugar_edit.setType(Update.class);
            case 7 -> sugar_edit.setType(UpdatePadding.class);
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
