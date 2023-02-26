package language.typing.types;

public class ParametricType implements Type {
    private String name;

    ParametricType() {
    }

    public ParametricType(String param) {
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
