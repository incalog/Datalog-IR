package language.typing.types;

import java.util.List;

public class TypeRef implements Type {
    private String name;
    private List<Type> paramTypes;

    TypeRef() {
    }

    public TypeRef(String newName) {
        name = newName;
    }
    
    TypeRef(String newName, List<Type> newTys) {
        name = newName;
        paramTypes = newTys;
    }

    @Override
    public String toString() {
        return name;
    }

    public String getName() {
        return name;
    }

    public List<Type> getParamTypes() {
        return paramTypes;
    }
}
