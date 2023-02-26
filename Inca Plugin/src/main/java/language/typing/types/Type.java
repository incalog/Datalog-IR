package language.typing.types;

import org.jetbrains.annotations.NotNull;

public interface Type {
    String toString();
    
    default boolean equals(@NotNull Type type) {
        return (this.toString().equals(type.toString()) && this.getClass().equals(type.getClass()));
    }

    default boolean equals(@NotNull String type) {
        return this.toString().equals(type);
    }

    default boolean isIntType() {
        return false;
    }
    default boolean isLongType() {
        return false;
    }
    default boolean isDoubleType() {
        return false;
    }
    default boolean isBooleanType() {
        return false;
    }
    default boolean isStringType() {
        return false;
    }
    default boolean isSetType() {return false;}

}
