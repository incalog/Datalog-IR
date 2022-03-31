package inca.abstractTreesitterAPI.util;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

public interface CLibrary extends Library {

    CLibrary INSTANCE = Native.load(Platform.isWindows() ? "msvcrt" : "c", CLibrary.class);
    CLibrary lib = INSTANCE;

    FILE fopen(String filename, String mode);
    int fclose(FILE stream);
}
