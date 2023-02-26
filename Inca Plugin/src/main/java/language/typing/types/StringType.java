package language.typing.types;

public class StringType implements Type {
    @Override
    public String toString() {
        return "String";
    }

    @Override
    public boolean isStringType() {
        return true;
    }
}
