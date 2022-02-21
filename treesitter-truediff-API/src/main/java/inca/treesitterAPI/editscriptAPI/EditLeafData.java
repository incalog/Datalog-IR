package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

@Structure.FieldOrder({"padding", "size", "lookahead_bytes", "parse_state", "boolean_values", "external_data"})
public class EditLeafData extends Structure {
    public Length padding;
    public Length size;
    public int lookahead_bytes;
    public short parse_state;
    public byte boolean_values; // packs values of has_external_tokens, depends_on_column and is_keyword into this field
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
