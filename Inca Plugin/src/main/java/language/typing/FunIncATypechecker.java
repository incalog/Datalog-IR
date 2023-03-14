package language.typing;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import language.FunIncAReference;
import language.psi.*;
import language.typing.types.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

public class FunIncATypechecker {


    // These two functions will return the type of the resolved target
    public static Type typeOfVarDef(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (element instanceof FunIncAFunDef) {
            FunIncAFunDef funDef = (FunIncAFunDef) element;
            Type returnType = new AnyType();
            if (funDef.getType() != null) {
                returnType = PsiToTypeConverter.convert(funDef.getType());
            }
            List<FunIncAType> paramTypesPsi = new ArrayList<>();
            for (FunIncAParamDef paramDef : funDef.getParamDefList()) {
                paramTypesPsi.add(paramDef.getType());
            }
            List<Type> paramTypes = PsiToTypeConverter.convert(paramTypesPsi);
            // TODO typeVariables
            List<Type> typeVars = new ArrayList<>();
            return new FunType(typeVars, paramTypes, returnType);
        } else if (element instanceof FunIncAParamDef) {
            FunIncAParamDef paramDef = (FunIncAParamDef) element;
            if (paramDef.getType() == null) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                                "Missing type annotation")
                        .range(paramDef.getId())
                        .create();
                return new AnyType();
            }
            return PsiToTypeConverter.convert(paramDef.getType());
        } else if (element instanceof FunIncADataConstructorDef) {
            FunIncADataConstructorDef constructorDef = (FunIncADataConstructorDef) element;
            List<Type> typeVariables = new ArrayList<>(); // TODO type variables
            List<Type> paramTypes = PsiToTypeConverter.convert(constructorDef.getTypeList());
            Type returnType = typeOfTypeDef(constructorDef.getParent(), holder);
            return new FunType(typeVariables, paramTypes, returnType);
        } else if (element instanceof FunIncAVarDef) {
            FunIncAVarDef varDef = (FunIncAVarDef) element;
            PsiElement parent = varDef.getParent();
            if (parent instanceof FunIncASingleBinding) {
                FunIncASingleBinding binding = (FunIncASingleBinding) parent;
                Type inferredType = typecheckExp(binding.getExpList().get(0), holder);
                if (binding.getType() == null) {
                    return inferredType;
                } else {
                    Type expectedType = PsiToTypeConverter.convert(binding.getType());
                    if (expectedType instanceof UnitType) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Cannot assign Type Unit to " + binding.getVarDef().getName())
                                .range(binding.getType())
                                .create();
                        expectedType = new AnyType();
                    }
                    if (!FunIncATypeUtil.subtype(inferredType, expectedType)) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Expected " + expectedType + ", but got " + inferredType)
                                .range(binding.getType())
                                .create();
                    }
                    return expectedType;
                }
            } else if (parent instanceof FunIncAMultipleBindings) {
                FunIncAMultipleBindings binding = (FunIncAMultipleBindings) parent;
                if (binding.getExpList() == null) { // has not been bound yet
                    return new AnyType();
                }
                Type inferredType = typecheckExp(binding.getExpList().get(0), holder);
                if (!(inferredType instanceof TupleType)) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Expected Tuple type, but got " + inferredType)
                            .range(binding.getExpList().get(0))
                            .create();
                    return new AnyType();
                }
                int index = binding.getVarDefList().indexOf(element); // position of the VarDef in question
                TupleType inferredTupleType = (TupleType) inferredType;
                if (binding.getType() == null) { // no expected types
                    if (index < inferredTupleType.getTypes().size())
                        return inferredTupleType.getTypes().get(index);
                    else
                        return new AnyType(); // nothing bound to that VarDef
                } else { // expected types are given
                    Type expectedType = PsiToTypeConverter.convert(binding.getType());
                    if (expectedType instanceof TupleType) {
                        TupleType expectedTupleType = (TupleType) expectedType;
                        if (index < expectedTupleType.getTypes().size()) { // this VarDef has an expected type
                            Type expectedTypeIndex = expectedTupleType.getTypes().get(index);
                            if (expectedTypeIndex instanceof UnitType) {
                                holder.newAnnotation(HighlightSeverity.ERROR,
                                                "Cannot assign Type Unit to "
                                                        + varDef.getName())
                                        .range(binding.getType().getAtomicType().getTupleType().getTypeList().get(index))
                                        .create();
                            }
                            if (index < inferredTupleType.getTypes().size()) {
                                Type inferredTypeIndex = inferredTupleType.getTypes().get(index);
                                if (!expectedTypeIndex.equals(inferredTypeIndex)) {
                                    holder.newAnnotation(HighlightSeverity.ERROR,
                                            "Expected " + expectedTypeIndex + ", but got " + inferredTypeIndex)
                                            .range(binding.getType().getAtomicType().getTupleType().getTypeList().get(index))
                                            .create();
                                }
                            } else { // VarDef has no inferred Type, but an expected type
                                holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Cannot infer type of " + varDef.getName())
                                        .range(varDef)
                                        .create();
                            }
                            return expectedTypeIndex;
                        } else { // this VarDef has no expected type despite given expected types, index > expectedType size
                            if (index < inferredTupleType.getTypes().size())
                                return inferredTupleType.getTypes().get(index);
                            else
                                return new AnyType();
                        }
                    } else { // expected type is not a tuple
                        if (index < inferredTupleType.getTypes().size())
                            return inferredTupleType.getTypes().get(index);
                        else
                            return new AnyType();
                    }
                }
            }
        }
        return new AnyType();
    }

    public static Type typeOfTypeDef(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (element instanceof FunIncATypeVarDef){
            // fundef or datadef
        }  else if (element instanceof FunIncADataDef) {
            FunIncADataDef dataDef = (FunIncADataDef) element;
            String name = dataDef.getName();
            List<Type> typeVars = new ArrayList<>();
            return new TypeRef(name);
        }
        holder.newAnnotation(HighlightSeverity.ERROR,
                "Unknown type reference")
                .range(element)
                .create();
        return new AnyType();
    }


    public static void validateType(@NotNull FunIncAType type, @NotNull AnnotationHolder holder) {
        PsiElement ty;
        try {
            ty = type.getFirstChild();
        } catch (NullPointerException e) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                    "Missing type")
                    .range(type)
                    .create();
            return;
        }
        if (ty instanceof FunIncAFunType) {
            FunIncAFunType funType = (FunIncAFunType) ty;
            validateAtomicType(funType.getAtomicType(), holder);
            validateType(funType.getType(), holder);
        } else { // ty is instance of AtomicType
            validateAtomicType((FunIncAAtomicType) ty, holder);
        }
    }

    public static void validateTypes(@Nullable List<FunIncAType> types, @NotNull AnnotationHolder holder) {
        if (types == null)
            return;
        for (FunIncAType type : types)
            validateType(type, holder);
    }

    private static void validateAtomicType(@NotNull FunIncAAtomicType type, @NotNull AnnotationHolder holder) {
        PsiElement ty;
        try {
            ty = type.getFirstChild();
        } catch (NullPointerException e) {
            holder.newAnnotation(HighlightSeverity.ERROR,
                            "Missing type")
                    .range(type)
                    .create();
            return;
        }
        String tyText = ty.getText();
        if (tyText.equals("Any") || tyText.equals("Nothing") || tyText.equals("Unit")) {
            return;
        } else if (ty instanceof FunIncAPrimitiveType) {
            return;
        }if (ty instanceof FunIncATupleType) {
            for (FunIncAType t : ((FunIncATupleType) ty).getTypeList())
                validateType(t, holder);
            return;
        } else if (ty instanceof FunIncASetType) {
            validateType(((FunIncASetType) ty).getType(), holder);
            return;
        } else if (ty instanceof FunIncAConstructorType || ty instanceof FunIncATypeNameRef) {
            FunIncAReference ref = (FunIncAReference) ty.getReference();
            ResolveResult[] result = ref.multiResolve(true);
            if (result.length == 0) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                                "Type " + tyText + " is not defined")
                        .range(ty)
                        .create();
                return;
            } else if (result.length == 1) {
                return;
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR,
                                "Ambiguos reference name " + tyText)
                        .range(ty)
                        .create();
                return;
            }
        } else {
            holder.newAnnotation(HighlightSeverity.ERROR,
                            "Unknown type " + tyText)
                    .range(ty)
                    .create();
            return;
        }
    }

    @NotNull
    public static Type typecheckExp(FunIncAExp exp, @NotNull AnnotationHolder holder) {
        if (exp instanceof FunIncAVarRefExp) {
            // call getTypeOfVarRef
            FunIncAVarRefExp varExp = (FunIncAVarRefExp) exp;
            FunIncAReference reference = (FunIncAReference) varExp.getReference();
            ResolveResult[] result = reference.multiResolve(true);
            if (result.length == 0) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Unresolved name " + varExp.getName())
                        .range(varExp)
                        .create();
                return new AnyType();
            } else if (result.length == 1){
                if (result[0].isValidResult()) {
                    Type resolvedType = typeOfVarDef(result[0].getElement(), holder);
                    return resolvedType;
                } else {
                    return new AnyType();
                }
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR, "Ambiguous reference name " + varExp.getName())
                        .range(varExp)
                        .create();
                return new AnyType();
            }

        } else if (exp instanceof FunIncALetExp) {
            FunIncALetExp letExp = (FunIncALetExp) exp;
            if (letExp.getSingleBinding() != null) {
                FunIncASingleBinding single = letExp.getSingleBinding();
                if (single.getType() != null)
                    validateType(single.getType(), holder);
                if (single.getExpList() == null || single.getExpList().size() == 1) { // if letExp doesn't have a body return Any
                    return new AnyType();
                } else {
                    FunIncAExp body = single.getExpList().get(single.getExpList().size() - 1);
                    return typecheckExp(body, holder);
                }
            } else if (letExp.getMultipleBindings() != null) {
                FunIncAMultipleBindings multiple = letExp.getMultipleBindings();
                if (multiple.getType() != null) {
                    validateType(multiple.getType(), holder);
                    Type type = PsiToTypeConverter.convert(multiple.getType());
                    if (type instanceof TupleType) {
                        int n = ((TupleType) type).getTypes().size();
                        int m = multiple.getVarDefList().size();
                        if (n != m)
                            holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Cannot assign " + n + "-ary tuple to " + m + " variables")
                                    .range(multiple.getType())
                                    .create();
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                "Cannot assign expression of type " + type + " to ")
                                .range(multiple.getType())
                                .create();
                    }
                }
                if (multiple.getExpList() == null || multiple.getExpList().size() == 1) { // if letExp doesn't have a body return Any
                    return new AnyType();
                } else {
                    FunIncAExp body = multiple.getExpList().get(multiple.getExpList().size() - 1);
                    return typecheckExp(body, holder);
                }
            } else {
                return new AnyType();
            }

        } else if (exp instanceof FunIncACastExp) {
            FunIncACastExp cast = (FunIncACastExp) exp;
            FunIncAExp ex = cast.getExp();
            Type expectedType = new TypeRef(Objects.requireNonNull(cast.getTypeNameRef()).getText());
            Type inferredType = typecheckExp(ex, holder);
            if (FunIncATypeUtil.meet(expectedType, inferredType).equals(new NothingType()))
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Type cast of " + expectedType + " is not compatible with inferred type " + inferredType + " of e " + exp).
                        range(exp.getTextRange()).create();
            return expectedType;

        } else if (exp instanceof FunIncAIfExp) {
            FunIncAIfExp ifExp = (FunIncAIfExp) exp;
            FunIncAExp cond = ifExp.getExpList().get(0);
            Type condType = typecheckExp(cond, holder);
            if (!(condType instanceof BooleanType)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Expected Boolean condition, but got " + condType)
                        .range(cond)
                        .create();
            }
            Type thenType = typecheckExp(ifExp.getExpList().get(1), holder);
            Type elseType = typecheckExp(ifExp.getExpList().get(2), holder);
            return FunIncATypeUtil.join(thenType, elseType);
        } else if (exp instanceof FunIncAParenthesisExp) {
            return typecheckExp(((FunIncAParenthesisExp) exp).getExp(), holder);
        } else if (exp instanceof FunIncATupleExp) {
            List<Type> tupleTypes = new ArrayList<>();
            for (FunIncAExp e : ((FunIncATupleExp) exp).getExpList())
                tupleTypes.add(typecheckExp(e, holder));
            return new TupleType(tupleTypes);
        } else if (exp instanceof FunIncALambdaExp) {
            FunIncALambdaExp lambdaExp = (FunIncALambdaExp) exp;
            FunIncAExp body = lambdaExp.getExp();
            Type returnType = new AnyType();
            if (body != null)
                returnType = typecheckExp(body, holder);
            List<FunIncAType> paramTypesPsi = new ArrayList<>();
            for (FunIncAParamDef paramDef : lambdaExp.getParamDefList()) {
                validateType(paramDef.getType(), holder);
                paramTypesPsi.add(paramDef.getType());
            }
            List<Type> paramTypes = PsiToTypeConverter.convert(paramTypesPsi);
            return new FunType(new ArrayList<>(), paramTypes, returnType);

        } else if (exp instanceof FunIncACallExp) {
            FunIncACallExp callExp = (FunIncACallExp) exp;
            FunIncAExp funExp = callExp.getExp();
            List<FunIncAExp> argExps = callExp.getCallExpListList().get(0).getExpList();
            if (funExp == null) {
                // TODO quick fix
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Missing function expression for function call")
                        .range(exp)
                        .create();
                return new AnyType();
            }
            Type funExpType = typecheckExp(funExp, holder);
            Type returnType = funExpType;
            int n = callExp.getCallExpListList().size(); // n holds the number of function calls
            while (n > 0) {
                argExps = callExp.getCallExpListList().get(callExp.getCallExpListList().size() - n).getExpList();
                if (returnType instanceof FunType) {
                    FunType funType = (FunType) returnType;
                    List<Type> argTypes = argExps.stream().map(e -> typecheckExp(e, holder)).collect(Collectors.toList());
                    if (funType.paramTypes.size() != argTypes.size()) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Expected " + funType.paramTypes.size() + " arguments, but got "
                                                + argTypes.size())
                                .range(callExp.getCallExpListList().get(callExp.getCallExpListList().size() - n))
                                .create();
                    } else {
                        for (int i = 0; i < funType.paramTypes.size(); i++) {
                            if (!FunIncATypeUtil.subtype(argTypes.get(i), funType.paramTypes.get(i))) {
                                holder.newAnnotation(HighlightSeverity.ERROR,
                                                "Expected argument of type " + funType.paramTypes.get(i)
                                                        + ", but got argument of type " + argTypes.get(i))
                                        .range(argExps.get(i))
                                        .create();
                            }
                        }
                    }
                    returnType = funType.returnType;
                }  else {
                    int beginning = callExp.getTextOffset();
                    int end = argExps.get(0).getTextOffset();
                    TextRange warningRange = new TextRange(beginning, end);
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Expected function type at function position of call, but got "
                                            + returnType)
                            .range(warningRange)
                            .create();
                    return returnType;
                }
                n -= 1;
            }
            return returnType;

        } else if (exp instanceof FunIncAMatchExp) {
//            FuncIncaExp matchee = ((FuncIncaMatchExp) exp).getExp();
//            List<FuncIncaMatchCase> cases = ((FuncIncaMatchExp) exp).getMatchCaseList();
//            FuncIncaType matcheeType = typecheck(matchee, holder);
//            if (matcheeType instanceof FunIncATypeRef) {
//                PsiElement tName = PsiTreeUtil.getChildOfType(matchee, FuncIncaVar.class);
//                if (tName.getReference() != null) { // tName is already defined
//                    FuncIncaType tNameType = typecheckTypeNameMatch(exp, cases, tName.getReference().resolve(), holder);
//                    if (tNameType instanceof FuncIncaParameterizedType){
//                        holder.newAnnotation(HighlightSeverity.ERROR, "Cannot match on parametric type " + tNameType).
//                                range(matchee.getTextRange()).create();
//                        List<FuncIncaType> caseTypes = new ArrayList<>();
//                        for (FuncIncaMatchCase matchCase : cases)
//                            caseTypes.add(typecheck(matchCase.getExp(), holder));
//                        return join(caseTypes);
//                    }
//                    return tNameType;
//                }
//            } else if (matcheeType instanceof FuncIncaConstructorType) {
//                PsiElement constrName = PsiTreeUtil.findChildOfType(matchee, FuncIncaVar.class); // TODO find correct definition of the datatype
//                if (constrName.getReference() != null)
//                    return typecheckConstructorMatch(exp, cases, constrName.getReference().resolve(), matcheeType, holder);
//            } else {
//                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot match on type " + matcheeType).
//                        range(matchee.getTextRange()).create();
//                List<FuncIncaType> caseTypes = new ArrayList<>();
//                for (FuncIncaMatchCase matchCase : cases)
//                    caseTypes.add(typecheck(matchCase.getExp(), holder));
//                return join(caseTypes);
//            }
        } else if (exp instanceof FunIncALiteralExp) {
            PsiElement e = exp.getFirstChild();
            if (e instanceof FunIncAIntLit) {
                return new IntType();
            } else if (e instanceof FunIncALongLit) {
                return new LongType();
            } else if (e instanceof FunIncADoubleLit) {
                return new DoubleType();
            } else if (e instanceof FunIncABooleanLit) {
                return new BooleanType();
            } else if (e instanceof FunIncAStringLit) {
                return new StringType();
            } else {
                // type scalaterm
            }
        } else if (exp instanceof FunIncABaseApplyExp) {

        } else if (exp instanceof FunIncABaseApplyUnaryExp) {
            FunIncABaseApplyUnaryExp unaryExp = (FunIncABaseApplyUnaryExp)  exp;
            String op = unaryExp.getUnaryOp().getText();
            Type eType = typecheckExp(unaryExp.getExp(), holder);
            switch (op) {
                case "-":
                    if (eType.isIntType() || eType.isDoubleType() || eType.isLongType()) {
                        return eType;
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR, "Arithmetic operator - cannot be used" +
                                        " with type " + eType + ".")
                                .range(exp)
                                .create();
                        return new AnyType();
                    }
                case "!":
                    if (eType.isBooleanType()) {
                        return eType;
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR, "Logic operator ! cannot be used" +
                                        " with type " + eType + ".")
                                .range(exp)
                                .create();
                        return new AnyType();
                    }
                default:
                    holder.newAnnotation(HighlightSeverity.ERROR, "Operation " + op + " is not supported.")
                            .range(exp)
                            .create();
                    return new AnyType();
            }
        } else if (exp instanceof FunIncABaseApplyMethodExp) {
            // TODO
        } else if (exp instanceof FunIncABaseApplyInfixExp) {
            FunIncABaseApplyInfixExp infixExp = (FunIncABaseApplyInfixExp) exp;
            List<FunIncAExp> children = infixExp.getExpList();
            String op = infixExp.getBinaryOp().getText();
            Type lhsType = typecheckExp(children.get(0), holder);
            if (children.size() == 1)
                return lhsType;
            Type rhsType = typecheckExp(children.get(1), holder);
            switch (op) {
                case "+":
                    return checkBinArithmeticOp(lhsType, rhsType, op, exp, holder);
                case "-":
                    return checkBinArithmeticOp(lhsType, rhsType, op, exp, holder);
                case "*":
                    return checkBinArithmeticOp(lhsType, rhsType, op, exp, holder);
                case "/":
                    return checkBinArithmeticOp(lhsType, rhsType, op, exp, holder);
                case "%":
                    return checkBinArithmeticOp(lhsType, rhsType, op, exp, holder);
                case "&&":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "||":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "<":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case ">":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "==":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "!=":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "<=":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case ">=":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "++":
                    return checkBinSetOp(lhsType, rhsType, op, exp, holder);
                case "&":
                    return checkBinSetOp(lhsType, rhsType, op, exp, holder);
                default:
                    holder.newAnnotation(HighlightSeverity.ERROR, "Operation " + op + " is not supported.")
                            .range(exp)
                            .create();
                    return new AnyType();
            }

        } else if (exp instanceof FunIncAConstSetExp) {
            List<FunIncAExp> items = ((FunIncAConstSetExp) exp).getExpList();
            List<Type> setTypes = new ArrayList<>();
            Type setType = null;
            if (items.size() != 0) {
                for (FunIncAExp item : items) {
                    Type itemType = typecheckExp(item, holder);
                    setTypes.add(itemType);
                }
                setType = FunIncATypeUtil.join(setTypes);
            }
            return new SetType(setType);
        } else if (exp instanceof FunIncASetMemberExp) {
            FunIncASetMemberExp memberExp = (FunIncASetMemberExp) exp;
            FunIncAExp tuple = memberExp.getExpList().get(0);
            FunIncAExp set = memberExp.getExpList().get(1);
            Type setType = typecheckExp(set, holder);
            Type setContentType;
            if (!(setType instanceof SetType)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Required set type, but got " + setType)
                        .range(set)
                        .create();
                setContentType = new AnyType();
            } else {
                setContentType = ((SetType) setType).getSetType();
            }

            Type typeTuple = typecheckExp(tuple, holder);
            if (typeTuple instanceof TupleType && setContentType instanceof TupleType) {
                int n = ((TupleType) typeTuple).getTypes().size();
                int m = ((TupleType) setContentType).getTypes().size();
                List<FunIncAExp> tupleExp = ((FunIncATupleExp) tuple).getExpList();
                if (n != m)
                    holder.newAnnotation(HighlightSeverity.ERROR,
                            "Set contains " + m + "-ary tuples, but test expression is " + n + "-ary")
                            .range(memberExp)
                            .create();
                for (int i = 0; i < Math.min(n,m); i++) {
                    Type expType = ((TupleType) typeTuple).getTypes().get(i);
                    Type setTupleType = ((TupleType) setContentType).getTypes().get(i);
                    if (! FunIncATypeUtil.subtype(expType, setTupleType))
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                "Expected " + setTupleType + ", but got " + expType)
                                .range(tupleExp.get(i))
                                .create();
                }
            }
            if (! FunIncATypeUtil.subtype(typeTuple, setContentType))
                holder.newAnnotation(HighlightSeverity.ERROR,
                                "Expected " + setContentType + ", but got " + typeTuple)
                        .range(tuple)
                        .create();
            return new BooleanType();

        } else if (exp instanceof FunIncASetComprehensionExp) {
            // TODO
        } else if (exp instanceof FunIncAFoldExp) {
            // TODO
        }
        return new AnyType();
    }


//    private static FuncIncaType typecheckTypeNameMatch(PsiElement exp,
//                                                       List<FunIncAMatchCase> cases,
//                                                       PsiElement tName, // the part of PSI tree, where this data got defined
//                                                       AnnotationHolder holder) {
//
//        Map<String, Integer> seenConstr = new HashMap<>();
//        Map<String, Integer> availableConstr = new HashMap<>();
//        for (FuncIncaDataConstructor c : ((FuncIncaDataDef) tName).getDataConstructorList())
//            availableConstr.put(c.getId().getText(), c.getTypeAnnotationList().size());
//
//        List<FuncIncaType> caseTypes = new ArrayList<>();
//        for (FuncIncaMatchCase matchCase : cases){
//            FuncIncaPattern pattern = matchCase.getPattern();
//            PsiElement e = matchCase.getExp();
//            if (pattern instanceof FuncIncaConstructorPattern) {
//                int params = ((FuncIncaConstructorPattern) pattern).getConsPatternIdList().size();
//                FuncIncaConsId consId = ((FuncIncaConstructorPattern) pattern).getConsId();
//                if (seenConstr.containsKey(consId.getText()))
//                    holder.newAnnotation(HighlightSeverity.ERROR, "Duplicate constructor pattern " + consId).
//                            range(consId.getTextRange()).create();
//                else
//                    seenConstr.put(consId.getText(), params);
//
//                for (Map.Entry<String, Integer> ac : availableConstr.entrySet()){
//                    if (seenConstr.containsKey(ac.getKey())) {
//                        if (params != ac.getValue())
//                            holder.newAnnotation(HighlightSeverity.ERROR,
//                                    "Wrong number of constructor arguments, expected " + ac.getValue() +
//                                            " but got " + params).range(pattern).create();
//                        // TODO scopedTypeContext {
//                        //              vars.zipAll(paramTypes, null, null).foreach {
//                        //                case (null, ty) => // nothing
//                        //                case (v, null) => bindVar(v.name, pat, TAny)
//                        //                case (v, ty) => bindVar(v.name, pat, ty)
//                        //              }
//                        //              typecheck(e)
//                        //            }
//                        caseTypes.add(typecheck(e, holder));
//                    } else {
//                        holder.newAnnotation(HighlightSeverity.ERROR,
//                                "Cannot match constructor " + consId + " against matchee of type " +
//                                        ((FuncIncaDataDef) tName).getId()).range(consId).create();
//                        // TODO scopedTypeContext {
//                        //              vars.foreach(v => bindVar(v.name, pat, TAny))
//                        //              typecheck(e)
//                        //            }
//                        caseTypes.add(typecheck(e, holder));
//                    }
//                }
//            } else { // pattern is not instance of FuncIncaConstructorPattern
//                holder.newAnnotation(HighlightSeverity.ERROR,"Cannot match pattern " + pattern +
//                        " against matchee of type " + ((FuncIncaDataDef) tName).getId()).range(pattern).create();
//                // TODO scopedTypeContext {
//                //          val dummy = ConstructorPattern(Name("?"), Seq())
//                //          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
//                //          typecheck(e)
//                //        }
//                caseTypes.add(typecheck(e, holder));
//            }
//        }
//
//        Set<String> missingCons = new HashSet<>();
//        for (Map.Entry<String, Integer> ac : availableConstr.entrySet())
//            if (!seenConstr.containsKey(ac.getKey()))
//                missingCons.add(ac.getKey());
//        holder.newAnnotation(HighlightSeverity.ERROR,
//                "Pattern must be complete but missed the following constructors: " + missingCons)
//                .range(exp).create();
//        return join(caseTypes);
//    }
//
//    private static FuncIncaType typecheckConstructorMatch(PsiElement exp, // matchee is a FuncIncaConstructorType
//                                                          List<FunIncAMatchCase> cases,
//                                                          PsiElement consName, // definition of this TypeConstructor matchee
//                                                          FuncIncaType matcheeType, // seperate, bc types do not reference their definition
//                                                          AnnotationHolder holder) {
//        PsiElement data = consName;
//        Map<String, DataConstructor> seenConstr = new HashMap<>();
//        Map<String, DataConstructor> availableConstr = new HashMap<>();
//        Map<String, FuncIncaType> subst = new HashMap<>(); // maps name of parameterized type to the currently used type
//
//        for (int i = 0; i < ((FuncIncaConstructorType) matcheeType).getTypes().size(); i++) {
//            FuncIncaType ty = ((FuncIncaConstructorType) matcheeType).getTypes().get(i);
//            String param = ((FunIncADataDef) consName).getTypeVarDefList().get(i).getId().getText();
//            subst.put(param, ty);
//        }
//
//        for (FuncIncaDataConstructor c : ((FuncIncaDataDef) data).getDataConstructorList()) {
//            List<FuncIncaType> tyVars = new ArrayList<>();
//            for (FuncIncaTypeVariable paramTy : c.getTypeVariables().getTypeVariableList())
//                tyVars.add(subst.get(paramTy.getText()));
//            availableConstr.put(c.getId().getText(),
//                    new DataConstructor(c.getId().getText(),
//                            tyVars,
//                            FuncIncaTypeUtil.psiToFuncIncaType(c.getTypeAnnotationList())));
//        }
//
//        List<FuncIncaType> caseTypes = new ArrayList<>();
//        for (FuncIncaMatchCase matchCase : cases) {
//            FuncIncaPattern pattern = matchCase.getPattern();
//            PsiElement e = matchCase.getExp();
//            if (pattern instanceof FuncIncaConstructorPattern) {
//                int params = ((FuncIncaConstructorPattern) pattern).getConsPatternIdList().size(); // number of parameters in patternmatch
//                FuncIncaConsId consId = ((FuncIncaConstructorPattern) pattern).getConsId();
//                if (seenConstr.containsKey(consId.getText()))
//                    holder.newAnnotation(HighlightSeverity.ERROR, "Duplicate constructor pattern " + consId).
//                            range(consId.getTextRange()).create();
//                else
//                    seenConstr.put(consId.getText(), new DataConstructor(consId.getText(),
//                            FuncIncaTypeUtil.typeVariablesToFuncIncaType(((FuncIncaConstructorPattern) pattern).getTypeVariables().getTypeVariableList()),
//                            Collections.nCopies(params, new FuncIncaAnyType())));
//
//                // TODO line 466
//                for (Map.Entry<String, DataConstructor> ac : availableConstr.entrySet()) {
//                    if (seenConstr.containsKey(ac.getKey())) {
//                        if (params != ac.getValue().getTypes().size())
//                            holder.newAnnotation(HighlightSeverity.ERROR,
//                                    "Wrong number of constructor aruments, expected " + ac.getValue().getTypes().size()
//                            + " but got " + params).range(pattern).create();
//                        // TODO scopedTypeContext {
//                        //              vars.zipAll(paramTypes, null, null).foreach {
//                        //                case (null, ty) => // nothing
//                        //                case (v, null) => bindVar(v.name, pat, TAny)
//                        //                case (v, ty) => bindVar(v.name, pat, ty)
//                        //              }
//                        //              typecheck(e)
//                        //            }
//                        caseTypes.add(typecheck(e, holder));
//                    } else {
//                        holder.newAnnotation(HighlightSeverity.ERROR,
//                                "Cannot match constructor " + consId + " against matchee of type " + matcheeType)
//                                .range(pattern)
//                                .create();
//                        // TODO scopedTypeContext {
//                        //              vars.foreach(v => bindVar(v.name, pat, TAny))
//                        //              typecheck(e)
//                        //            }
//                    }
//                }
//            } else { // pattern is not instance of FuncIncaConstructorPattern
//                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot match pattern " + pattern
//                        + " against type " + consName).range(pattern).create();
//                // TODO scopedTypeContext {
//                //          val dummy = ConstructorPattern(Name("?"), Seq())
//                //          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
//                //          typecheck(e)
//                //        }
//                caseTypes.add(typecheck(e, holder));
//            }
//        }
//        Set<String> missingCons = new HashSet<>();
//        for (Map.Entry<String, DataConstructor> ac : availableConstr.entrySet())
//            if (!seenConstr.containsKey(ac.getKey()))
//                missingCons.add(ac.getKey());
//        holder.newAnnotation(HighlightSeverity.ERROR,
//                        "Pattern must be complete but missed the following constructors: " + missingCons)
//                .range(exp).create();
//        return join(caseTypes);
//    }

    private static Type checkBinArithmeticOp(Type lhs, Type rhs, String op, PsiElement exp, AnnotationHolder holder) {
        if (lhs.isIntType()) {
            if (rhs.isIntType() || rhs.isDoubleType() || rhs.isLongType()) {
                return rhs;
            } else { // rhs not a numeric type
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                                " with types " + lhs + " and " + rhs)
                        .range(exp)
                        .create();
                return new AnyType();
            }
        } else if (lhs.isDoubleType()) {
            if (rhs.isIntType() || rhs.isDoubleType()) {
                return lhs;
            } else if (rhs.isLongType()) {
                return new DoubleType();
            } else { // rhs not a numeric type
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                                " with types " + lhs + " and " + rhs)
                        .range(exp)
                        .create();
                return new AnyType();
            }
        } else if (lhs.isLongType()) {
            if (rhs.isIntType() || rhs.isLongType()) {
                return lhs;
            } else if (rhs.isDoubleType()) {
                return new DoubleType();
            } else { // rhs is not a numeric type
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                                " with types " + lhs + " and " + rhs)
                        .range(exp)
                        .create();
                return new AnyType();
            }
        } else { // lhs not a numeric type
            holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                            " with types " + lhs + " and " + rhs)
                    .range(exp)
                    .create();
            return new AnyType();
        }
    }

    private static Type checkBinLogicOp(Type lhs, Type rhs, String op, PsiElement exp, AnnotationHolder holder) {
        if (lhs.isBooleanType() && rhs.isBooleanType())
            return lhs;
        else {
            holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use logic operator " + op +
                            " with types " + lhs + " and " + rhs)
                    .range(exp)
                    .create();
            return new AnyType();
        }
    }

    private static Type checkBinSetOp(Type lhs, Type rhs, String op, PsiElement exp, AnnotationHolder holder) {
        String opName = "unknown set operation";
        if (op.equals("++")) opName = "union";
        if (op.equals("&")) opName = "intersetion";
        if (lhs.isSetType() && rhs.isSetType()){
            Type lhsSetType = ((SetType) lhs).getSetType();
            Type rhsSetType = ((SetType) rhs).getSetType();
            if (lhsSetType.equals(rhsSetType)) {
                return lhs;
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot perform " + opName + " on sets holding " +
                                "different types: " + lhsSetType + " and " + rhsSetType + ".")
                        .range(exp)
                        .create();
                return new AnyType();
            }
        } else {
            holder.newAnnotation(HighlightSeverity.ERROR, "Cannot perform " + opName + " on non set types: "
                            + lhs + " and " + rhs + ".")
                    .range(exp)
                    .create();
            return new AnyType();
        }
    }
}
