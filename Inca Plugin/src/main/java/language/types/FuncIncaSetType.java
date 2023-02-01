package language.types;

public class FuncIncaSetType implements FuncIncaType {
    private FuncIncaType setType;

    FuncIncaSetType() {
    }

    FuncIncaSetType(FuncIncaType ty) {
        setType = ty;
    }

    public FuncIncaType getSetType() {
        return setType;
    }

    @Override
    public String toString() {
        return "Set[" + setType.toString() + "]";
    }

    @Override
    public boolean isSetType() {
        return true;
    }
}
