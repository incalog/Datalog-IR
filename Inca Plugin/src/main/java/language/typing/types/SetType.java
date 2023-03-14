package language.typing.types;

public class SetType implements Type {
    private Type setType;

    SetType() {
    }

    public SetType(Type ty) {
        setType = ty;
    }

    public Type getSetType() {
        return setType;
    }

    @Override
    public String toString() {
        if (setType == null)
            return "Set[]";
        return "Set[" + setType.toString() + "]";
    }

    @Override
    public boolean isSetType() {
        return true;
    }
}
