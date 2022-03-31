package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*C definition:
* typedef struct {
    bool is_leaf: 1;                Bit Packing. Use type closest to original type size to group the values in JNA.
    TSSymbol tag;                   TSSymbol: alias for uint16_t.
    void *id;
    ChildPrototypeArray kids;
} Load;
*/

@Structure.FieldOrder({"is_leaf", "tag", "id", "kids"})
public class Load extends Structure {

    public byte is_leaf; // packs value of is_leaf into this field.
    public short tag;
    public Pointer id;
    public ChildPrototypeArray kids;

    public Load() {
        super();
    }

    public Load(Pointer p) {
        super(p);
        this.read();
    }
}
