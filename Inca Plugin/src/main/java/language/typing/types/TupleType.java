package language.typing.types;

import java.util.List;

public class TupleType implements Type {
    final public List<Type> types;

    public TupleType(List<Type> tys) {
        types = tys;
    }

    @Override
    public String toString() {
        if (types == null)
            return "()";
        String str = "(";
        for (Type ty : types)
            str += ty.toString() + ", ";
        return str.substring(0, str.length() - 2) + ")";
    }
}
