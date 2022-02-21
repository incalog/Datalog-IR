package inca.treesitterAPI.editscriptAPI;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.Union;

@Structure.FieldOrder({"data", "length"})
public class ExternalScannerState extends Structure {
    public Data data;
    public int length;

    public static class Data extends Union {
        public Pointer long_data;
        public byte[] short_data = new byte[24];
    }

    @Override
    public void read() {
        super.read();
        if (length <= 24) {
            data.setType("short_data");
        } else {
            data.setType(Pointer.class);
        }
        data.read();
    }

    public ExternalScannerState() {
        super();
    }

    public ExternalScannerState(Pointer p) {
        super(p);
        this.read();
    }
}
