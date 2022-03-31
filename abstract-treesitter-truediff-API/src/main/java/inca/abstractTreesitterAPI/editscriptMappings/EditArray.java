package inca.abstractTreesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef Array(SugaredEdit) EditArray;
*
* Array(T) is alias for following definition:
* #define Array(T)       \
  struct {             \
    T *contents;       \
    uint32_t size;     \
    uint32_t capacity; \
  }
*/

@Structure.FieldOrder({"content", "size", "capacity"})
public class EditArray extends Structure {

    // Use ByValue for function arguments and return values.
    public static class ByValue extends EditArray implements Structure.ByValue { }

    public SugaredEdit.ByReference content;
    public int size;
    public int capacity;

    public EditArray() {
        super();
    }

    public EditArray(Pointer p) {
        super(p);
        this.read();
    }

    // Ease creation of SugarEdit array.
    public SugaredEdit.ByReference[] toArray(int size) {
        return (SugaredEdit.ByReference[]) content.toArray(size);
    }
}
