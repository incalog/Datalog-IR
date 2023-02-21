package language.types;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import language.util.Pair;
import language.psi.*;
import org.apache.commons.lang.ObjectUtils;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class TypeContext {
    private static TypeContext instance = new TypeContext();
    private static Map<String, Pair<FuncIncaDecl, FuncIncaType>> vars; // name -> (declaration, type)
    private static Map<String, FuncIncaTypeVariable> tyVars; // name -> declaration
    private static Map<String, List<Pair<FuncIncaDecl, FuncIncaType>>> funDefs; //
    private static Map<String, FuncIncaDataDef> dataDefs; // name -> declaration

    private TypeContext() {
        vars = new HashMap<>();
        tyVars = new HashMap<>();
        funDefs = new HashMap<>();
        dataDefs = new HashMap<>();
    }

    public TypeContext getInstance() {
        return instance;
    }

    // ------------------------------------- context stuff ------------------------------------
    
    public static void scopedTypeContext(Runnable runnable) {
        Map<String, Pair<FuncIncaDecl, FuncIncaType>> varsSaved = new HashMap<>(vars);
        Map<String, FuncIncaTypeVariable> tyVarsSaved = new HashMap<>(tyVars);
        Map<String, List<Pair<FuncIncaDecl, FuncIncaType>>> funDefsSaved = new HashMap<>(funDefs);

        runnable.run();

        vars = varsSaved;
        tyVars = tyVarsSaved;
        funDefs = funDefsSaved;
    }

    public static void bindVar(String name, FuncIncaDecl decl, FuncIncaType type, AnnotationHolder holder) {
        Pair<FuncIncaDecl, FuncIncaType> prevDecl = vars.put(name, new Pair<>(decl, type));
        if (prevDecl != null) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                            "Variable " + name + " shadows previously defined variable")
                    .range(decl)
                    .create();
            // holder.newAnnotation(HighlightSeverity.ERROR,
            //                 "Variable " + name + " shadowed by later declaration")
            //         .range(prevDecl.getFirst())
            //         .create();
        }
    }

    public static @Nullable Pair<FuncIncaDecl, FuncIncaType> lookupVar(String name, FuncIncaVar var, AnnotationHolder holder) {
        Pair<FuncIncaDecl, FuncIncaType> v = vars.get(name);
        if (v == null) { // name not found in vars
            List<Pair<FuncIncaDecl, FuncIncaType>> funList = funDefs.get(name);
            if (funList == null) { // name not found in funDefs
                holder.newAnnotation(HighlightSeverity.ERROR,"Unbound variable " + name)
                        .range(var)
                        .create();
            } else { // name found in funDefs
                if (funList.size() == 1) {
                    v = funList.get(0);
                } else {
                    holder.newAnnotation(HighlightSeverity.ERROR, "Ambiguous call to " + name)
                            .range(var)
                            .create();
                }
            }
        }
        return v;
    }

    public static boolean isFreeVar(String name) {
        return !vars.containsKey(name);
    }

    public static void bindTyVar(String name, FuncIncaTypeVariable decl, AnnotationHolder holder) {
        PsiElement prevDecl = tyVars.put(name, decl);
        if (prevDecl != null) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                    "Type Variable " + name + " shadows previously defined Type variable")
                    .range(decl)
                    .create();
            holder.newAnnotation(HighlightSeverity.ERROR,
                    "Type Variable " + name + " shadowed by later declaration")
                    .range(prevDecl)
                    .create();
        }
    }

    public static FuncIncaTypeVariable lookupTyVar(String name) {
        return tyVars.get(name);
    }

    public static boolean isTypeVar(String name) {
        return tyVars.containsKey(name);
    }

    public static void bindFun(FuncIncaFunDef funDef) {
        String name = funDef.getId().getText();
        List<FuncIncaParam> params;
        List<FuncIncaTypeVariable> tVars;
        try {
            params = funDef.getParamList().getParamList();
        } catch (NullPointerException e) {
            params = new ArrayList<>();
        }
        try {
            tVars = funDef.getTypeVariables().getTypeVariableList();
        } catch (NullPointerException e) {
            tVars = null;
        }
        FuncIncaTypeAnnotation res = funDef.getTypeAnnotation();

        List<FuncIncaType> paramTypes = new ArrayList<>();
        for (FuncIncaParam param : params) {
            FuncIncaType type = FuncIncaTypeUtil.psiToFuncIncaType(param.getTypeAnnotation());
            paramTypes.add(type);
        }
        List<FuncIncaType> typeVariables = FuncIncaTypeUtil.typeVariablesToFuncIncaType(tVars);
        FuncIncaType returnType = FuncIncaTypeUtil.psiToFuncIncaType(res);
        FuncIncaFunctionType funType = new FuncIncaFunctionType(typeVariables, paramTypes, returnType);

        addFunDef(name, new Pair<>(funDef, funType));
    }

    private static void addFunDef(String name, Pair<FuncIncaDecl, FuncIncaType> pair) {
        List<Pair<FuncIncaDecl, FuncIncaType>> v = funDefs.get(name);
        if (v == null) { // no function of that name exists yet
            funDefs.put(name, Arrays.asList(pair));
        } else { // there are already functions with that name
            ArrayList<Pair<FuncIncaDecl, FuncIncaType>> newValue = new ArrayList<>(v);
            newValue.add(pair);
            funDefs.put(name, newValue);
        }
    }

    public static @Nullable Pair<FuncIncaDecl, FuncIncaType> lookupCalled(String name, FuncIncaCallExp call, AnnotationHolder holder) {
        List<Pair<FuncIncaDecl, FuncIncaType>> funList = funDefs.get(name);
        Pair<FuncIncaDecl, FuncIncaType> v = null;
        if (funList.isEmpty()) { // name was not found in funDefs
            v = vars.get(name);
            if (v == null) { // name was not found in vars
                holder.newAnnotation(HighlightSeverity.ERROR, "Unbound name " + name)
                        .range(call)
                        .create();
            } else { // name was found in vars
                FuncIncaType ty = v.getSecond();
                if (!(ty instanceof FuncIncaFunctionType)) {
                    holder.newAnnotation(HighlightSeverity.ERROR, "Variable " + name + " has type " + ty +
                                    ", but required function type")
                            .range(call)
                            .create();
                }
            }
        } else { // name was found in funDef
            if (funList.size() == 1) {
                v = funList.get(0);
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR, "Ambiguous call to " + name)
                        .range(call)
                        .create();
            }
        }

        return v;
    }

    public static void bindData(FuncIncaDataDef data) {
        // add new data definition
        String dataName = data.getName();
        dataDefs.put(dataName, data);
        // add constructors to function definitions
        List<FuncIncaType> dataTyVars;
        try {
            dataTyVars = FuncIncaTypeUtil.typeVariablesToFuncIncaType(data.getTypeVariables().getTypeVariableList());
        } catch (NullPointerException e) {
            dataTyVars = new ArrayList<>();
        }
        FuncIncaType d = new FuncIncaTypeNameType(dataName, dataTyVars);
        List<FuncIncaDataConstructor> cs = data.getDataConstructorList();
        for (FuncIncaDataConstructor c : cs) {
            String cName = c.getName(); // TODO: catch nullpointer, annotations für missing type variables
            List<FuncIncaType> tvs = new ArrayList<>();
            List<FuncIncaType> vs = new ArrayList<>();
            if (c.getTypeVariables() != null) // does this DataConstructor c have type variables
                tvs = FuncIncaTypeUtil.typeVariablesToFuncIncaType(c.getTypeVariables().getTypeVariableList());
            if (c.getTypeAnnotationList() != null) // does this DataConstructor c have parameters
                vs = FuncIncaTypeUtil.psiToFuncIncaType(c.getTypeAnnotationList());
            FuncIncaFunctionType constr = new FuncIncaFunctionType(tvs, vs, d);
            addFunDef(cName, new Pair<>(c, constr));
        }
    }
    // data.constrs.foreach(c => funs += c.name -> (module, (c, c.constructorType(data))))

    public static boolean isData(String name) {
        return dataDefs.containsKey(name);
    }

    public static boolean isDataOrTypeVar(String name) {
        return dataDefs.containsKey(name) || tyVars.containsKey(name);
    }

    public static FuncIncaDataDef lookupData(String name) {
        return dataDefs.get(name);
    }

}
