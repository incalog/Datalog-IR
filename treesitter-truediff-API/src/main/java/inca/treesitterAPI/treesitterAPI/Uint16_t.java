package inca.treesitterAPI.treesitterAPI;

import com.sun.jna.IntegerType;

public class Uint16_t extends IntegerType {
    public Uint16_t() {
        this(0);
    }

    public Uint16_t(int value) {
        super(2, value, true);
    }
}