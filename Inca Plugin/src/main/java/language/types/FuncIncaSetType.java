package language.types;

public class FuncIncaSetType {
    private FuncIncaType setType;

    FuncIncaSetType(){}

    FuncIncaSetType(FuncIncaType ty){
        setType = ty;
    }

    public FuncIncaType getSetType(){
        return setType;
    }

}
