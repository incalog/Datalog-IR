package inca.abstractTreesitterAPI.treesitterMappings;

import com.sun.jna.IntegerType;

// Custom class for uint32_t data type.
public class Uint32_t extends IntegerType {

    public Uint32_t() {
        this(0);
    }

    public Uint32_t(int value) {
        super(4, value, true);
    }
}