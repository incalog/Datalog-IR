package language.types;

import java.util.List;

public class FuncIncaConstructorType implements FuncIncaType {
    private String name;
    private List<FuncIncaType> types;

    public FuncIncaConstructorType(String newName, List<FuncIncaType> newTypes){
        name = newName;
        types = newTypes;
    }

    @Override
    public String toString(){
        String str = name + "[";
        for (FuncIncaType type : types){
            str += type.toString() + ",";
        }
        return str.substring(0, str.length() - 1) + "]";
    }

    public String getName() {
        return name;
    }

    public List<FuncIncaType> getTypes(){
        return types;
    }
}
