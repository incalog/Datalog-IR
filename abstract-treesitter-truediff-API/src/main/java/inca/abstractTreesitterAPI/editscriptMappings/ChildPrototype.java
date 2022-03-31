package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*
* C definition:
* typedef struct {
    void *child_id;
    bool is_field;              Decides whether child is unnamed or not.
    union {
        TSFieldId field_id;     TSFieldId: alias for uint16_t.
        uint32_t link;
    };
} ChildPrototype;
*/

@Structure.FieldOrder({"child_id", "is_field", "child_name"})
public class ChildPrototype extends Structure {

    // Use ByReference for pointer in struct fields.
    public static class ByReference extends ChildPrototype implements Structure.ByReference { }

    public Pointer child_id;
    public byte is_field;
    public ChildName child_name;

    public static class ChildName extends Union {
        public short field_id;
        public int link;
    }

    // Use custom read() function to set appropriate type for union field field_link.
    @Override
    public void read() {
        super.read();
        if (is_field == 1) {
            child_name.setType("field_id");
        } else {
            child_name.setType("link");
        }
        child_name.read();
    }

    public ChildPrototype() {
        super();
    }

    public ChildPrototype(Pointer p) {
        super(p);
        this.read();
    }
}
