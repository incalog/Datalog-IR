package inca.treesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef struct{
  void *id;
  TSSymbol tag;             TSSymbol: alias for uint16_t.
  Length old_padding;
  Length new_padding;
} UpdatePadding;
*/

@Structure.FieldOrder({"id", "tag", "old_padding", "new_padding"})
public class UpdatePadding extends Structure {

    public Pointer id;
    public short tag;
    public Length old_padding;
    public Length new_padding;

    public UpdatePadding() {
        super();
    }

    public UpdatePadding(Pointer p) {
        super(p);
        this.read();
    }
}
