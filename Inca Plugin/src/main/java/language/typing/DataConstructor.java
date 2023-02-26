package language.typing;

import language.typing.types.Type;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class DataConstructor {
    private String name;
    private List<Type> tyVars;
    private List<Type> types;

    DataConstructor() {}
    
    DataConstructor(String newName) {
        name = newName;
    }

    DataConstructor(String newName, List<Type> newTypes) {
        name = newName;
        types = newTypes;
    }

    DataConstructor(String newName, List<Type> newTyVars, @Nullable List<Type> newTypes) {
        name = newName;
        tyVars = newTyVars;
        types = newTypes;
    }

    public String getName() {
        return name;
    }

    public List<Type> getTyVars() {
        return tyVars;
    }

    public List<Type> getTypes() {
        return types;
    }

    public void setTyVars(List<Type> tyVars) {
        this.tyVars = tyVars;
    }

    public void setTypes(List<Type> types) {
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
