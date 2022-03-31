package inca.abstractTreesitterAPI.treesitterMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* struct TSLiteralMap {
    uint32_t symbol_count;
    uint8_t *symbol_map;        Size of symbol_map: (lit_map->symbol_count / 8) + 1
    uint8_t *unnamed_tokens;    Size of unnamed_tokens: (lit_map->symbol_count / 8) + 1
};
*/

@Structure.FieldOrder({"symbol_count", "symbol_map", "unnamed_tokens"})
public class TSLiteralMap extends Structure {

    public int symbol_count;
    public Pointer symbol_map;
    public Pointer unnamed_tokens;

    public int ts_literal_map_is_literal(short symbol) {
        byte[] byteArray = this.symbol_map.getByteArray(0, (symbol_count / 8) + 1);
        return byteArray[symbol / 8] & (1 << (symbol % 8));
    }

    public int ts_literal_map_is_unnamed_token(short symbol) {
        byte[] byteArray = this.unnamed_tokens.getByteArray(0, (symbol_count / 8) + 1);
        return byteArray[symbol / 8] & (1 << (symbol % 8));
    }

    public TSLiteralMap() {
        super();
    }

    public TSLiteralMap(Pointer p) {
        super(p);
        this.read();
    }
}