package language.types;

import org.jetbrains.annotations.Nullable;

import java.util.List;

public class DataConstructor {
    private String name;
    private List<FuncIncaType> tyVars;
    private List<FuncIncaType> types;

    DataConstructor() {}
    
    DataConstructor(String newName) {
        name = newName;
    }

    DataConstructor(String newName, List<FuncIncaType> newTypes) {
        name = newName;
        types = newTypes;
    }

    DataConstructor(String newName, List<FuncIncaType> newTyVars, @Nullable List<FuncIncaType> newTypes) {
        name = newName;
        tyVars = newTyVars;
        types = newTypes;
    }

    public String getName() {
        return name;
    }

    public List<FuncIncaType> getTyVars() {
        return tyVars;
    }

    public List<FuncIncaType> getTypes() {
        return types;
    }

    public void setTyVars(List<FuncIncaType> tyVars) {
        this.tyVars = tyVars;
    }

    public void setTypes(List<FuncIncaType> types) {
        this.types = types;
    }

    @Override
    public String toString() {
        if (tyVars == null) 
            return name + "(" + types + ")";
        else
            return name + "[" + tyVars + "](" + types +")";
    }
}
