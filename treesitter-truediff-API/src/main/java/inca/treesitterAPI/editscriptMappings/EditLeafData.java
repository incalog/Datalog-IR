package inca.treesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

/*
* C definition:
* typedef struct {
    Length padding;
    Length size;
    uint32_t lookahead_bytes;
    TSStateId parse_state;          TSStateId: alias for uint16_t.
    bool has_external_tokens: 1;    Bit Packing. Use type close to original type size to map packed values.
    bool depends_on_column: 1;
    bool is_keyword: 1;
    union {
        ExternalScannerState external_scanner_state;
        int32_t lookahead_char;
    };
} EditLeafData;
*/

@Structure.FieldOrder({"padding", "size", "lookahead_bytes", "parse_state", "boolean_values", "external_data"})
public class EditLeafData extends Structure {

    public Length padding;
    public Length size;
    public int lookahead_bytes;
    public short parse_state;
    public byte boolean_values; // packs values of has_external_tokens, depends_on_column and is_keyword into this field.
    public ExternalData external_data;

    public static class ExternalData extends Union {

        public ExternalScannerState external_scanner_state;
        public int lookahead_char;
    }

    public EditLeafData() {
        super();
    }

    public EditLeafData(Pointer p) {
        super(p);
        this.read();
    }
}
