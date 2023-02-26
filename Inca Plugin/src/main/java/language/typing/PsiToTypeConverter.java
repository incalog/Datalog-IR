package language.typing;

import language.psi.*;
import com.intellij.psi.PsiElement;
import language.psi.FunIncASetType;
import language.psi.FunIncATupleType;
import language.psi.FunIncAType;
import language.typing.types.*;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class PsiToTypeConverter {
    /**
     * Takes a list of parameters as PSI Elements and returns their types as FuncIncaTypes
     * @param tyVars
     * @return
     */
//    public static List<language.typing.types.FunIncAType> convert(List<FunIncATypeNameRef> tyVars) {
//        List<language.typing.types.FunIncAType> res = new ArrayList<>();
//        if (tyVars == null || tyVars.isEmpty()) {
//            return res;
//        }
//        // TODO leere liste oder null.
//        // TODO wie soll eine leere parametermenge gewertet werden? () Unit, Nothing wohl nicht, oder einfach Liste leer lassen in Typdefinition?
//
//        for (FunIncATypeNameRef tyVar : tyVars)
//            res.add(new language.typing.types.FunIncAParametricType(tyVar.getText()));
//        return res;
//    }

    /**
     * Takes one Type Annotation as a PSI element and returns it as a FuncIncaType
     * @param element
     * @return
     */
    public static Type convert(FunIncAType element) {
        PsiElement e;
        try {
            e = element.getFirstChild();
        }
        catch (NullPointerException np) {
            return new AnyType();
        }
        if (e instanceof FunIncAFunType) {
            Type argType = convert(((FunIncAFunType) e).getAtomicType());
            Type returnType = convert(((FunIncAFunType) e).getType());
            if (argType instanceof TupleType) {
                return new FunType(new ArrayList<>(), ((TupleType) argType).getTypes(), returnType);
            } else {
                return new FunType(new ArrayList<>(), List.of(argType), returnType);
            }
        } else { // e is instance of FuncIncaAtomicType
            return convert((FunIncAAtomicType) e);
        }
    }

    public static List<Type> convert(@Nullable List<FunIncAType> elements) {
        List<Type> types = new ArrayList<>();
        if (null == elements || elements.isEmpty())
            return types;
        for (FunIncAType e : elements)
            types.add(convert(e));
        return types;
    }


    private static Type convert(FunIncAAtomicType element){
        PsiElement e = element.getFirstChild();
        String eText = e.getText();
        if (e instanceof FunIncATupleType) {
            return new TupleType(convert(((FunIncATupleType) e).getTypeList()));
        } else if (eText.equals("Any")) {
            return new AnyType();
        } else if (eText.equals("Nothing")) {
            return new NothingType();
        } else if (eText.equals("Unit")) {
            return new UnitType();
        } else if (e instanceof FunIncASetType) {
            return new SetType(convert(((FunIncASetType) e).getType()));
        } else if (e instanceof FunIncAConstructorType) {
            FunIncAConstructorType constrType = (FunIncAConstructorType) e;
            String name = constrType.getTypeNameRef().getText();
            return new ConstructorType(name, convert(constrType.getTypeList()));
        } else if (eText.equals("Boolean")) {
            return new BooleanType();
        } else if (eText.equals("Double")) {
            return new DoubleType();
        } else if (eText.equals("Int")) {
            return new IntType();
        } else if (eText.equals("Long")) {
            return new LongType();
        } else if (eText.equals("String")) {
            return new StringType();
        } else if (e instanceof FunIncATypeNameRef){
            return new TypeRef(eText);
        } else {
            return new AnyType();
        }
    }
}
