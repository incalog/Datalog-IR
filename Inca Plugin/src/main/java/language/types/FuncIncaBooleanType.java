package language.types;

public class FuncIncaBooleanType implements FuncIncaType {
    @Override
    public String toString() {
        return "Boolean";
    }

    @Override
    public boolean isBooleanType() {
        return true;
    }
}
