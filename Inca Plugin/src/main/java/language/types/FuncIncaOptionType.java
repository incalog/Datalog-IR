package language.types;

public class FuncIncaOptionType implements FuncIncaType {
    private FuncIncaType type;
    
    public FuncIncaOptionType(FuncIncaType t) {
        type = t;
    }
    
    public FuncIncaType getType() {
        return type;
    }

    @Override
    public String toString() {
        return "Option[" + type + "]";
    }
}
