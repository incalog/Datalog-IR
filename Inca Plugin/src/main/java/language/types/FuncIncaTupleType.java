package language.types;

import org.jetbrains.annotations.NotNull;

import java.util.List;

public class FuncIncaTupleType {
    private List<FuncIncaType> types;

    FuncIncaTupleType() {}

    FuncIncaTupleType(List<FuncIncaType> tys){
        types = tys;
    }

    public List<FuncIncaType> getTypes(){
        return types;
    }
    
}
