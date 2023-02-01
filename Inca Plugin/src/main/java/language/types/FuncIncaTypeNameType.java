package language.types;

import java.util.List;

public class FuncIncaTypeNameType implements FuncIncaType {
    private String name;
    private List<FuncIncaType> paramTypes;

    FuncIncaTypeNameType() {
    }

    FuncIncaTypeNameType(String newName) {
        name = newName;
    }
    
    FuncIncaTypeNameType(String newName, List<FuncIncaType> newTys) {
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

    public List<FuncIncaType> getParamTypes() {
        return paramTypes;
    }
}
