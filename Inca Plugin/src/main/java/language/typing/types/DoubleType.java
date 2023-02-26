package language.typing.types;

public class DoubleType implements Type {
    @Override
    public String toString() {
        return "Double";
    }

    @Override
    public boolean isDoubleType() {
        return true;
    }
}
