package language.types;

import java.util.List;

public class FuncIncaFunctionType implements FuncIncaType {

    public final List<FuncIncaType> typeVars;
    public final List<FuncIncaType> paramTypes;
    public final FuncIncaType returnType;

    FuncIncaFunctionType(List<FuncIncaType> typeVars, List<FuncIncaType> paramTypes, FuncIncaType returnType) {
        this.typeVars = typeVars;
        this.paramTypes = paramTypes;
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        if (paramTypes.isEmpty())
            return "() => " + returnType;
        String args = "";
        for (FuncIncaType param : paramTypes) {
            if (param instanceof FuncIncaFunctionType)
                args += "(" + param + "), ";
            else
                args += param + ", ";
        }

        return "(" + args.substring(0, args.length() - 2) + ") => " + returnType;
    }
}
