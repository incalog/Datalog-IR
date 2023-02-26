package language.typing.types;

import java.util.List;

public class TupleType implements Type {
    private List<Type> types;

    TupleType() {
    }

    public TupleType(List<Type> tys) {
        types = tys;
    }

    public List<Type> getTypes() {
        return types;
    }

    @Override
    public String toString() {
        String str = "(";
        for (Type ty : types)
            str += ty.toString() + ", ";
        return str.substring(0, str.length() - 2) + ")";
    }
}
