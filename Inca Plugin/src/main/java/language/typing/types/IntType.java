package language.typing.types;

public class IntType implements Type {
    @Override
    public String toString() {
        return "Int";
    }

    @Override
    public boolean isIntType() {
        return true;
    }
}
