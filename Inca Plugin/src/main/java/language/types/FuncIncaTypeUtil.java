package language.types;

import com.intellij.psi.PsiElement;
import language.psi.*;
import org.apache.commons.compress.utils.Lists;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class FuncIncaTypeUtil {
    /**
     * Takes a list of parameters as PSI Elements and returns their types as FuncIncaTypes
     * @param tyVars
     * @return
     */
    public static List<FuncIncaType> typeVariablesToFuncIncaType(List<FuncIncaTypeVariable> tyVars) {
        List<FuncIncaType> res = new ArrayList<>();
        if (tyVars == null || tyVars.isEmpty()) {
            return res;
        }
        // TODO leere liste oder null.
        // TODO wie soll eine leere parametermenge gewertet werden? () Unit, Nothing wohl nicht, oder einfach Liste leer lassen in Typdefinition?

        for (FuncIncaTypeVariable tyVar : tyVars)
            res.add(new FuncIncaParameterizedType(tyVar.getText()));
        return res;
    }

    /**
     * Takes one Type Annotation as a PSI element and returns it as a FuncIncaType
     * @param element
     * @return
     */
    public static FuncIncaType psiToFuncIncaType(FuncIncaTypeAnnotation element) {
        PsiElement e;
        try {
            e = element.getFirstChild();
        }
        catch (NullPointerException np) {
            return new FuncIncaAnyType();
        }
        if (e instanceof FuncIncaFunType) {
            FuncIncaType argType = atomicTypeToFuncIncaType(((FuncIncaFunType) e).getAtomicType());
            FuncIncaType returnType = psiToFuncIncaType(((FuncIncaFunType) e).getTypeAnnotation());
            if (argType instanceof FuncIncaTupleType) {
                return new FuncIncaFunctionType(new ArrayList<>(), ((FuncIncaTupleType) argType).getTypes(), returnType);
            } else {
                return new FuncIncaFunctionType(new ArrayList<>(), Arrays.asList(argType), returnType);
            }
        } else { // e is instance of FuncIncaAtomicType
            return atomicTypeToFuncIncaType((FuncIncaAtomicType) e);
        }
    }

    public static List<FuncIncaType> psiToFuncIncaType(@Nullable List<FuncIncaTypeAnnotation> elements) {
        List<FuncIncaType> types = new ArrayList<>();
        if (null == elements || elements.isEmpty())
            return types;
        for (FuncIncaTypeAnnotation e : elements)
            types.add(psiToFuncIncaType(e));
        return types;
    }


    private static FuncIncaType atomicTypeToFuncIncaType(FuncIncaAtomicType element){
        PsiElement e = element.getFirstChild();
        String eText = e.getText();
        if (e instanceof FuncIncaTuple) {
            return new FuncIncaTupleType(psiToFuncIncaType(((FuncIncaTuple) e).getTypeAnnotationList()));
        } else if (eText.equals("Any")) {
            return new FuncIncaAnyType();
        } else if (eText.equals("Nothing")) {
            return new FuncIncaNothingType();
        } else if (eText.equals("Unit")) {
            return new FuncIncaUnitType();
        } else if (e instanceof FuncIncaSet) {
            return new FuncIncaSetType(psiToFuncIncaType(((FuncIncaSet) e).getTypeAnnotation()));
        } else if (e instanceof FuncIncaConstr) {
            List<FuncIncaTypeAnnotation> annos = ((FuncIncaConstr) e).getTypeAnnotationList();
            return new FuncIncaConstructorType(((FuncIncaConstr) e).getVar().getText(), psiToFuncIncaType(annos));
        } else if (eText.equals("Boolean")) {
            return new FuncIncaBooleanType();
        } else if (eText.equals("Double")) {
            return new FuncIncaDoubleType();
        } else if (eText.equals("Int")) {
            return new FuncIncaIntegerType();
        } else if (eText.equals("Long")) {
            return new FuncIncaLongType();
        } else if (eText.equals("String")) {
            return new FuncIncaStringType();
        } else if (e instanceof FuncIncaTypeNameType){
            return new FuncIncaTypeNameType(eText);
        } else {
            return new FuncIncaAnyType();
        }
    }
}
