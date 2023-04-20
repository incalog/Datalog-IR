package language.typing.types;

import java.util.List;

public class TypeRef implements Type {

    public final String name;

    public TypeRef(String newName) {
        name = newName;
    }

    @Override
    public String toString() {
        return name;
    }

}
