package inca.abstractTreesitterAPI.treesitterMappings;

import com.sun.jna.IntegerType;

// Custom class for uint16_t data type.
public class Uint16_t extends IntegerType {

    public Uint16_t() {
        this(0);
    }

    public Uint16_t(int value) {
        super(2, value, true);
    }
}