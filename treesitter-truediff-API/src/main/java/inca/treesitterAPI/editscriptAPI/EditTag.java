package inca.treesitterAPI.editscriptAPI;

public interface EditTag {
    public static final int ATTACH = 0;
    public static final int DETACH = 1;
    public static final int UNLOAD = 2;
    public static final int LOAD = 3;
    public static final int LOAD_ATTACH = 4;
    public static final int DETACH_UNLOAD = 5;
    public static final int UPDATE = 6;
    public static final int UPDATE_PADDING = 7;
}
