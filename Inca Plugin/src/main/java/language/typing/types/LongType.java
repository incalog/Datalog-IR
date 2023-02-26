package language.typing.types;

public class LongType implements Type {
    @Override
    public String toString() {
        return "Long";
    }

    @Override
    public boolean isLongType() {
        return true;
    }
}
