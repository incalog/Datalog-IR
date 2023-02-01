package language.types;

public class FuncIncaDoubleType implements FuncIncaType {
    @Override
    public String toString() {
        return "Double";
    }

    @Override
    public boolean isDoubleType() {
        return true;
    }
}
