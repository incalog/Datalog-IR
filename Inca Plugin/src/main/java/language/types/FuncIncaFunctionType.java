package language.types;

import java.util.List;

public class FuncIncaFunctionType {
    private List<FuncIncaType> args;
    private FuncIncaType res;
    
    FuncIncaFunctionType(){}
    
    FuncIncaFunctionType(FuncIncaType r, List<FuncIncaType> a){
        args = a;
        res = r;
    }
    
    public List<FuncIncaType> getArgs(){
        return args;
    }
    
    public FuncIncaType getRes(){
        return res;
    }
}
