package language.types;

public class FuncIncaLongType implements FuncIncaType {
    @Override
    public String toString() {
        return "Long";
    }

    @Override
    public boolean isLongType() {
        return true;
    }
}
