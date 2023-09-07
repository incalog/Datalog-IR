package language.typing.types;

public class SetType implements Type {
    final public Type setType;

    public SetType(Type ty) {
        setType = ty;
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
