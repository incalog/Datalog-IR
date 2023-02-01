package language.types;

public class FuncIncaStringType implements FuncIncaType {
    @Override
    public String toString() {
        return "String";
    }

    @Override
    public boolean isStringType() {
        return true;
    }
}
