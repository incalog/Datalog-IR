package language.types;

import org.jetbrains.annotations.NotNull;

import java.util.List;

public class FuncIncaTupleType implements FuncIncaType {
    private List<FuncIncaType> types;

    FuncIncaTupleType() {
    }

    FuncIncaTupleType(List<FuncIncaType> tys) {
        types = tys;
    }

    public List<FuncIncaType> getTypes() {
        return types;
    }

    @Override
    public String toString() {
        String str = "(";
        for (FuncIncaType ty : types)
            str += ty.toString() + ",";
        return "Tuple" + str.substring(0, str.length() - 1) + ")";
    }
}
