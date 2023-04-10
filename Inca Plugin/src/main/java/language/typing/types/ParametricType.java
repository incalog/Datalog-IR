package language.typing.types;

public class ParametricType implements Type {
    public String name;

    public ParametricType(String param) {
        name = param;
    }

    @Override
    public String toString() {
        return name;
    }

}
