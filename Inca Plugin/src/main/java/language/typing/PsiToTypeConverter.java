package language.typing;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import language.FunIncAReference;
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
                return new FunType(new ArrayList<>(), ((TupleType) argType).types, returnType);
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

    public static List<Type> convertTypeVarDefs(@Nullable List<FunIncATypeVarDef> elements) {
        List<Type> types = new ArrayList<>();
        if (elements == null || elements.isEmpty())
            return types;
        else {
            FunIncATypeVarDef element = elements.remove(0);
            String name = element.getName();
            types.add(new ParametricType(name));
            types.addAll(convertTypeVarDefs(elements));
            return types;
        }
    }

    private static Type convert(FunIncAAtomicType element){
        PsiElement e = element.getFirstChild();
        String eText = e.getText();
        if (e instanceof FunIncATupleType) {
            List<FunIncAType> types = ((FunIncATupleType) e).getTypeList();
            return new TupleType(convert(types));
        } else if (eText.equals("Any")) {
            return new AnyType();
        } else if (eText.equals("Nothing")) {
            return new NothingType();
        } else if (eText.equals("Unit")) {
            return new UnitType();
        } else if (e instanceof FunIncASetType) {
            return new SetType(convert(((FunIncASetType) e).getType()));
        } else if (e instanceof FunIncAConstructorType) {
            FunIncAConstructorType constructorType = (FunIncAConstructorType) e;
            String name = constructorType.getTypeNameRef().getId().getText();
            List<Type> types = convert(constructorType.getTypeList());
            return new ConstructorType(name, types);
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
           String name = ((FunIncATypeNameRef) e).getId().getText();
           return new TypeRef(name, new ArrayList<>());
        } else {
            return new AnyType();
        }
    }
}
