package inca.treesitterAPI.editscriptAPI;

/*
* Used to map the EditTag enum:
* typedef enum {
    ATTACH, DETACH, UNLOAD, LOAD, LOAD_ATTACH, DETACH_UNLOAD, UPDATE, UPDATE_PADDING
} EditTag;
*/

public interface EditTag {
    int ATTACH = 0;
    int DETACH = 1;
    int UNLOAD = 2;
    int LOAD = 3;
    int LOAD_ATTACH = 4;
    int DETACH_UNLOAD = 5;
    int UPDATE = 6;
    int UPDATE_PADDING = 7;
}
