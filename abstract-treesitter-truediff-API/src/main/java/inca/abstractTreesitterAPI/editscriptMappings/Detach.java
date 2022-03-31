package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*
* C definition:
* typedef struct {
  void *id;
  TSSymbol tag;             TSSymbol: alias for uint16_t.
  void *parent_id;
  TSSymbol parent_tag;
  bool is_field;
  union {
    TSFieldId field_id;     TSFieldId: alias for uint16_t.
    uint32_t link;
  };
} Detach;
*/

@Structure.FieldOrder({"id", "tag", "parent_id", "parent_tag", "is_field", "field_link"})
public class Detach extends Structure {

    public Pointer id;
    public short tag;
    public Pointer parent_id;
    public short parent_tag;
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

    public Detach() {
        super();
    }

    public Detach(Pointer p) {
        super(p);
        this.read();
    }
}
