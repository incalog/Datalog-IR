package inca.treesitterAPI.treesitterMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* struct TSLiteralMap {
    uint32_t symbol_count;
    uint8_t *symbol_map;        Size of symbol_map: (lit_map->symbol_count / 8) + 1
};
*/

@Structure.FieldOrder({"symbol_count", "symbol_map"})
public class TSLiteralMap extends Structure {

    public int symbol_count;
    public Pointer symbol_map;

    public int ts_literal_map_is_literal(short symbol) {
        return symbol_map.getByteArray(4, (symbol_count / 8) + 1)[symbol / 8] & (1 << (symbol % 8));
    }

    public TSLiteralMap() {
        super();
    }

    public TSLiteralMap(Pointer p) {
        super(p);
        this.read();
    }
}