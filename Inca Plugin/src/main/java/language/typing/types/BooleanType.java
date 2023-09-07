package language.typing.types;

public class BooleanType implements Type {
    @Override
    public String toString() {
        return "Boolean";
    }

    @Override
    public boolean isBooleanType() {
        return true;
    }
}
