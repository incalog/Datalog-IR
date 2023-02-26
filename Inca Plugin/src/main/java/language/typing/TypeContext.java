//package language.typing;
//
//import com.intellij.lang.annotation.AnnotationHolder;
//import com.intellij.lang.annotation.HighlightSeverity;
//import com.intellij.psi.PsiElement;
//import language.util.Pair;
//import language.psi.*;
//import org.apache.commons.lang.ObjectUtils;
//import org.jetbrains.annotations.Nullable;
//
//import java.util.*;
//import java.util.stream.Collectors;
//
//public class TypeContext {
//    private static TypeContext instance = new TypeContext();
//    private static Map<String, Pair<FunIncADecl, FunIncAType>> vars; // name -> (declaration, type)
//    private static Map<String, FunIncATypeVarDef> tyVars; // name -> declaration
//    private static Map<String, List<Pair<FunIncADecl, FunIncAType>>> funDefs; //
//    private static Map<String, FunIncADataDef> dataDefs; // name -> declaration
//
//    private TypeContext() {
//        vars = new HashMap<>();
//        tyVars = new HashMap<>();
//        funDefs = new HashMap<>();
//        dataDefs = new HashMap<>();
//    }
//
//    public TypeContext getInstance() {
//        return instance;
//    }
//
//    // ------------------------------------- context stuff ------------------------------------
//
//    public static void scopedTypeContext(Runnable runnable) {
//        Map<String, Pair<FunIncADecl, FunIncAType>> varsSaved = new HashMap<>(vars);
//        Map<String, FunIncATypeVarDef> tyVarsSaved = new HashMap<>(tyVars);
//        Map<String, List<Pair<FunIncADecl, FunIncAType>>> funDefsSaved = new HashMap<>(funDefs);
//
//        runnable.run();
//
//        vars = varsSaved;
//        tyVars = tyVarsSaved;
//        funDefs = funDefsSaved;
//    }
//
//    public static void bindVar(String name, FunIncADecl decl, FunIncAType type, AnnotationHolder holder) {
//        Pair<FunIncADecl, FunIncAType> prevDecl = vars.put(name, new Pair<>(decl, type));
//        if (prevDecl != null) {
//            holder.newAnnotation(HighlightSeverity.ERROR,
//                            "Variable " + name + " shadows previously defined variable")
//                    .range(decl)
//                    .create();
//            // holder.newAnnotation(HighlightSeverity.ERROR,
//            //                 "Variable " + name + " shadowed by later declaration")
//            //         .range(prevDecl.getFirst())
//            //         .create();
//        }
//    }
//
//    public static @Nullable Pair<FunIncADecl, FunIncAType> lookupVar(String name, FunIncAVarRef var, AnnotationHolder holder) {
//        Pair<FunIncADecl, FunIncAType> v = vars.get(name);
//        if (v == null) { // name not found in vars
//            List<Pair<FunIncADecl, FunIncAType>> funList = funDefs.get(name);
//            if (funList == null) { // name not found in funDefs
//                holder.newAnnotation(HighlightSeverity.ERROR,"Unbound variable " + name)
//                        .range(var)
//                        .create();
//            } else { // name found in funDefs
//                if (funList.size() == 1) {
//                    v = funList.get(0);
//                } else {
//                    holder.newAnnotation(HighlightSeverity.ERROR, "Ambiguous call to " + name)
//                            .range(var)
//                            .create();
//                }
//            }
//        }
//        return v;
//    }
//
//    public static boolean isFreeVar(String name) {
//        return !vars.containsKey(name);
//    }
//
//    public static void bindTyVar(String name, FunIncATypeVarDef decl, AnnotationHolder holder) {
//        PsiElement prevDecl = tyVars.put(name, decl);
//        if (prevDecl != null) {
//            holder.newAnnotation(HighlightSeverity.ERROR,
//                    "Type Variable " + name + " shadows previously defined Type variable")
//                    .range(decl)
//                    .create();
//            holder.newAnnotation(HighlightSeverity.ERROR,
//                    "Type Variable " + name + " shadowed by later declaration")
//                    .range(prevDecl)
//                    .create();
//        }
//    }
//
//    public static FunIncATypeVarDef lookupTyVar(String name) {
//        return tyVars.get(name);
//    }
//
//    public static boolean isTypeVar(String name) {
//        return tyVars.containsKey(name);
//    }
//
//    public static void bindFun(FunIncAFunDef funDef) {
//        String name = funDef.getId();
//        List<FunIncAParamDef> params;
//        List<FunIncATypeVarDef> tVars;
//        try {
//            params = funDef.getParamList().getParamList();
//        } catch (NullPointerException e) {
//            params = new ArrayList<>();
//        }
//        try {
//            tVars = funDef.getTypeVariables().getTypeVariableList();
//        } catch (NullPointerException e) {
//            tVars = null;
//        }
//        FunIncATypeAnnotation res = funDef.getTypeAnnotation();
//
//        List<FunIncAType> paramTypes = new ArrayList<>();
//        for (FunIncAParam param : params) {
//            FunIncAType type = FunIncATypeUtil.psiToFunIncAType(param.getTypeAnnotation());
//            paramTypes.add(type);
//        }
//        List<FunIncAType> typeVariables = FunIncATypeUtil.typeVariablesToFunIncAType(tVars);
//        FunIncAType returnType = FunIncATypeUtil.psiToFunIncAType(res);
//        FunIncAFunctionType funType = new FunIncAFunctionType(typeVariables, paramTypes, returnType);
//
//        addFunDef(name, new Pair<>(funDef, funType));
//    }
//
//    private static void addFunDef(String name, Pair<FunIncADecl, FunIncAType> pair) {
//        List<Pair<FunIncADecl, FunIncAType>> v = funDefs.get(name);
//        if (v == null) { // no function of that name exists yet
//            funDefs.put(name, Arrays.asList(pair));
//        } else { // there are already functions with that name
//            ArrayList<Pair<FunIncADecl, FunIncAType>> newValue = new ArrayList<>(v);
//            newValue.add(pair);
//            funDefs.put(name, newValue);
//        }
//    }
//
//    public static @Nullable Pair<FunIncADecl, FunIncAType> lookupCalled(String name, FunIncACallExp call, AnnotationHolder holder) {
//        List<Pair<FunIncADecl, FunIncAType>> funList = funDefs.get(name);
//        Pair<FunIncADecl, FunIncAType> v = null;
//        if (funList.isEmpty()) { // name was not found in funDefs
//            v = vars.get(name);
//            if (v == null) { // name was not found in vars
//                holder.newAnnotation(HighlightSeverity.ERROR, "Unbound name " + name)
//                        .range(call)
//                        .create();
//            } else { // name was found in vars
//                FunIncAType ty = v.getSecond();
//                if (!(ty instanceof FunIncAFunctionType)) {
//                    holder.newAnnotation(HighlightSeverity.ERROR, "Variable " + name + " has type " + ty +
//                                    ", but required function type")
//                            .range(call)
//                            .create();
//                }
//            }
//        } else { // name was found in funDef
//            if (funList.size() == 1) {
//                v = funList.get(0);
//            } else {
//                holder.newAnnotation(HighlightSeverity.ERROR, "Ambiguous call to " + name)
//                        .range(call)
//                        .create();
//            }
//        }
//
//        return v;
//    }
//
//    public static void bindData(FunIncADataDef data) {
//        // add new data definition
//        String dataName = data.getName();
//        dataDefs.put(dataName, data);
//        // add constructors to function definitions
//        List<FunIncAType> dataTyVars;
//        try {
//            dataTyVars = FunIncATypeUtil.typeVariablesToFunIncAType(data.getTypeVariables().getTypeVariableList());
//        } catch (NullPointerException e) {
//            dataTyVars = new ArrayList<>();
//        }
//        FunIncAType d = new FunIncATypeRef(dataName, dataTyVars);
//        List<FunIncADataConstructor> cs = data.getDataConstructorList();
//        for (FunIncADataConstructor c : cs) {
//            String cName = c.getName(); // TODO: catch nullpointer, annotations für missing type variables
//            List<FunIncAType> tvs = new ArrayList<>();
//            List<FunIncAType> vs = new ArrayList<>();
//            if (c.getTypeVariables() != null) // does this DataConstructor c have type variables
//                tvs = FunIncATypeUtil.typeVariablesToFunIncAType(c.getTypeVariables().getTypeVariableList());
//            if (c.getTypeAnnotationList() != null) // does this DataConstructor c have parameters
//                vs = FunIncATypeUtil.psiToFunIncAType(c.getTypeAnnotationList());
//            FunIncAFunctionType constr = new FunIncAFunctionType(tvs, vs, d);
//            addFunDef(cName, new Pair<>(c, constr));
//        }
//    }
//    // data.constrs.foreach(c => funs += c.name -> (module, (c, c.constructorType(data))))
//
//    public static boolean isData(String name) {
//        return dataDefs.containsKey(name);
//    }
//
//    public static boolean isDataOrTypeVar(String name) {
//        return dataDefs.containsKey(name) || tyVars.containsKey(name);
//    }
//
//    public static FunIncADataDef lookupData(String name) {
//        return dataDefs.get(name);
//    }
//
//}
