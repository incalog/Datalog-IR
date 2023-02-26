package language.typing.types;

import java.util.List;

public class ConstructorType implements Type {
    private final String name;
    private final List<Type> types;

    public ConstructorType(String newName, List<Type> newTypes){
        name = newName;
        types = newTypes;
    }

    @Override
    public String toString(){
        StringBuilder str = new StringBuilder(name + "[");
        for (Type type : types){
            str.append(type.toString()).append(",");
        }
        return str.substring(0, str.length() - 1) + "]";
    }

    public String getName() {
        return name;
    }

    public List<Type> getTypes(){
        return types;
    }
}
