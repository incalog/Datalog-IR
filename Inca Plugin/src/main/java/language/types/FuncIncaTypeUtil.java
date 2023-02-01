package language.types;

import com.intellij.psi.PsiElement;
import language.psi.*;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class FuncIncaTypeUtil {
    public static List<FuncIncaType> paramTypeToFuncIncaType(List<FuncIncaParamType> params) {
        List<FuncIncaType> res = new ArrayList<>();
        for (FuncIncaParamType param : params)
            res.add(new FuncIncaParameterizedType(param.getText()));
        return res;
    }

    public static FuncIncaType psiToFuncIncaType(FuncIncaTypeAnnotation element) {
        PsiElement e = element.getFirstChild();
        if (e instanceof FuncIncaFunType) {
            return new FuncIncaFunctionType(
                    atomicTypeToFuncIncaType(((FuncIncaFunType) e).getAtomicType()),
                    psiToFuncIncaType(((FuncIncaFunType) e).getTypeAnnotation()));
        } else { // e is instance of FuncIncaAtomicType
            return atomicTypeToFuncIncaType((FuncIncaAtomicType) e);
        }
    }

    public static List<FuncIncaType> psiToFuncIncaType(@Nullable List<FuncIncaTypeAnnotation> elements) {
        if (null == elements)
            return null;
        List<FuncIncaType> types = new ArrayList<>();
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
        } else if (e instanceof FuncIncaOption) {
            return new FuncIncaOptionType(psiToFuncIncaType(((FuncIncaOption) e).getTypeAnnotation()));
        } else if (e instanceof FuncIncaSet) {
            return new FuncIncaSetType(psiToFuncIncaType(((FuncIncaSet) e).getTypeAnnotation()));
        } else if (e instanceof FuncIncaConstr) {
            List<FuncIncaTypeAnnotation> annos = ((FuncIncaConstr) e).getTypeAnnotationList();
            return new FuncIncaConstructorType(((FuncIncaConstr) e).getVar().getText(), psiToFuncIncaType(annos));
        } else if (e instanceof FuncIncaBooleanType) {
            return new FuncIncaBooleanType();
        } else if (e instanceof FuncIncaDoubleType) {
            return new FuncIncaDoubleType();
        } else if (e instanceof FuncIncaIntegerType) {
            return new FuncIncaIntegerType();
        } else if (e instanceof FuncIncaLongType) {
            return new FuncIncaIntegerType();
        } else if (e instanceof FuncIncaStringType) {
            return new FuncIncaStringType();
        } else { // e is instance of FuncIncaTypeName
            return new FuncIncaTypeNameType(eText);
        }
    }
}
