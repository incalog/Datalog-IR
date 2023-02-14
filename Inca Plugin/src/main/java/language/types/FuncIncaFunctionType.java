package language.types;

public class FuncIncaFunctionType implements FuncIncaType {
    private FuncIncaType arg;
    private FuncIncaType res;

    FuncIncaFunctionType() {
    }

    FuncIncaFunctionType(FuncIncaType a, FuncIncaType r) {
        arg = a;
        res = r;
    }

    public FuncIncaType getArg() {
        return arg;
    }

    public FuncIncaType getRes() {
        return res;
    }

    @Override
    public String toString() {
        return arg + " -> " + res;
    }
}
