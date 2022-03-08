package inca.treesitterAPI.editscriptMappings;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;

/*
* C definition:
* typedef Array(ChildPrototype) ChildPrototypeArray;
*
* Array(T): alias for following definition:
* #define Array(T)       \
  struct {             \
    T *contents;       \
    uint32_t size;     \
    uint32_t capacity; \
  }
*/

@Structure.FieldOrder({"content", "size", "capacity"})
public class ChildPrototypeArray extends Structure {

    public Pointer content;
    public int size;
    public int capacity;

    public ChildPrototypeArray() {
        super();
    }

    public ChildPrototypeArray(Pointer p) {
        super(p);
        this.read();
    }
}
