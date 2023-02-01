package language.types;

public class FuncIncaParameterizedType implements FuncIncaType {
    private String name;

    FuncIncaParameterizedType() {
    }

    FuncIncaParameterizedType(String param) {
        name = param;
    }

    @Override
    public String toString() {
        return name;
    }
    
    public String getName() {
        return name;
    }
}
