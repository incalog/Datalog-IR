package language.typing.types;

import java.util.List;

public class FunType implements Type {

    public final List<Type> typeVars;
    public List<Type> paramTypes;
    public Type returnType;

    public FunType(List<Type> typeVars, List<Type> paramTypes, Type returnType) {
        this.typeVars = typeVars;
        this.paramTypes = paramTypes;
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        if (paramTypes.isEmpty())
            return "() => " + returnType;
        StringBuilder args = new StringBuilder();
        for (Type param : paramTypes) {
            if (param instanceof FunType)
                args.append("(").append(param).append("), ");
            else
                args.append(param).append(", ");
        }

        return "(" + args.substring(0, args.length() - 2) + ") => " + returnType;
    }
}
