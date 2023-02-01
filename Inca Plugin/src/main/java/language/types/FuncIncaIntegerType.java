package language.types;

public class FuncIncaIntegerType implements FuncIncaType {
    @Override
    public String toString() {
        return "Int";
    }

    @Override
    public boolean isIntType() {
        return true;
    }
}
