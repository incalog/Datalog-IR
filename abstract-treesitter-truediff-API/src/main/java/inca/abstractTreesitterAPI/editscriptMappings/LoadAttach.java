package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*
* C definition:
* typedef struct {
  bool is_leaf: 1;              Bit Packing. Use type closest to original type size to group the values in JNA.
  void *id;
  void *parent_id;
  TSSymbol parent_tag;          TSSymbol: alias for uint16_t.
  TSSymbol tag;
  ChildPrototypeArray kids;
  bool is_field;
  union {
    TSFieldId field_id;
    uint32_t link;
  };
} LoadAttach;
*/

@Structure.FieldOrder({"is_leaf", "id", "parent_id", "parent_tag", "tag", "kids", "is_field", "field_link"})
public class LoadAttach extends Structure {

    public byte is_leaf; // Groups value of is_leaf into this field.
    public Pointer id;
    public Pointer parent_id;
    public short parent_tag;
    public short tag;
    public ChildPrototypeArray kids;
    public byte is_field;
    public FieldLink field_link;

    public static class FieldLink extends Union {

        public short field_id;
        public int link;
    }

    // Use custom read() function to set appropriate type for union field field_link.
    @Override
    public void read() {
        super.read();
        if (is_field == 1) {
            field_link.setType("field_id");
        } else {
            field_link.setType("link");
        }
        field_link.read();
    }

    public LoadAttach() {
        super();
    }

    public LoadAttach(Pointer p) {
        super(p);
        this.read();
    }
}
