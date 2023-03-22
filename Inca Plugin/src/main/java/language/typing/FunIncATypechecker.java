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
import org.apache.commons.lang.ObjectUtils;
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
        } else if (element instanceof FunIncAVarRefExp) { // only in set member exp in set comprehension
            FunIncAVarRefExp varRef = (FunIncAVarRefExp) element;
            FunIncASetMemberExp setMemberExp = PsiTreeUtil.getParentOfType(varRef, FunIncASetMemberExp.class);
            FunIncAExp exp = setMemberExp.getExpList().get(0);
            FunIncAExp set = setMemberExp.getExpList().get(1);
            boolean isTupleExp = exp.getFirstChild() instanceof FunIncATupleExp;
            Type type = typecheckExp(set, holder);
            if (!(type instanceof SetType))
                return new AnyType();
            SetType setType = (SetType) type;
            if (isTupleExp) {
                if (!(setType.getSetType() instanceof TupleType)) {
                    return new AnyType();
                }
                int index = ((FunIncATupleExp) exp).getExpList().indexOf(element);
                if (index < ((TupleType) setType.getSetType()).getTypes().size()) {
                    return ((TupleType) setType.getSetType()).getTypes().get(index);
                } else {
                    return new AnyType();
                }
            } else {
                return setType.getSetType();
            }

        } else if (element instanceof FunIncAPatternVarDef) {
            FunIncAPatternVarDef patternVarDef = (FunIncAPatternVarDef) element;
            FunIncAConstructorPat pattern = (FunIncAConstructorPat) element.getParent();
            FunIncAReference ref = (FunIncAReference) pattern.getConstructorRef().getReference();
            ResolveResult[] result = ref.multiResolve(true);
            FunIncADataConstructorDef constructorDef;
            if (result.length == 1) { // error messages in patterns are covered in typecheckExp
                if (result[0].isValidResult()) {
                    constructorDef = (FunIncADataConstructorDef) result[0].getElement();
                } else {
                    return new AnyType();
                }
            } else { // 0 results, or 1< results
                return new AnyType();
            }
            int index = pattern.getPatternVarDefList().indexOf(patternVarDef);
            List<FunIncAType> typeList = constructorDef.getTypeList();
            if (typeList.size() > index) {
                return PsiToTypeConverter.convert(typeList.get(index));
            } else {
                return new AnyType();
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
            List<Type> typeVars = new ArrayList<>(); // TODO type vars
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
                if ((PsiTreeUtil.getParentOfType(varExp, FunIncASetMemberExp.class) != null) &&
                        (PsiTreeUtil.getParentOfType(varExp, FunIncASetComprehensionExp.class) != null)) {
                    // if varExp is in a setMemberExpression within a SetComprehensionExp it is a declaration
                    return typeOfVarDef(varExp, holder);
                } else {
                    holder.newAnnotation(HighlightSeverity.ERROR, "Unresolved name " + varExp.getName())
                            .range(varExp)
                            .create();
                    return new AnyType();
                }
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
                        "Type cast of " + expectedType + " is not compatible with inferred type " +
                                inferredType + " of expression " + cast.getExp().getText()).
                        range(exp.getTextRange())
                        .create();
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
                    String callExpText = callExp.getText();
                    int callTextLength = callExpText.indexOf("(");
                    int beginning = callExp.getTextRange().getStartOffset();
                    int end = beginning + callTextLength - 1;
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
            FunIncAMatchExp matchExp = (FunIncAMatchExp) exp;
            FunIncAExp matchee = matchExp.getExp();
            Type matcheeType = typecheckExp(matchee, holder);
            List<FunIncAMatchCase> cases = matchExp.getMatchCaseList();
            if (matcheeType instanceof TypeRef) {
                String dataDefName = matcheeType.toString();
                PsiElement root = matchee.getContainingFile();
                List<PsiElement> rootChildren = List.of(root.getChildren());
                List<FunIncADataDef> dataDefs = new ArrayList<>();
                for (PsiElement child : rootChildren) {
                    if (child instanceof FunIncADataDef && dataDefName.equals(((FunIncADataDef) child).getName()))
                        dataDefs.add((FunIncADataDef) child);
                }
                if (dataDefs.isEmpty()) {
                    // TypeRef can only reference data definitions, or parametric types.
                    // So when there is no data definition found, TypeRef references a parametric type.
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Cannot match on parametric type " + matcheeType)
                            .range(matchee)
                            .create();
                    List<Type> caseTypes = new ArrayList<>();
                    for (FunIncAMatchCase matchCase : cases)
                        caseTypes.add(typecheckExp(matchCase.getExp(), holder));
                    return FunIncATypeUtil.join(caseTypes);
                } else if (dataDefs.size() > 1) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Cannot match on type " + matcheeType + ", ambiguous definition of type " +
                                    matcheeType)
                            .range(matchee)
                            .create();
                    List<Type> caseTypes = new ArrayList<>();
                    for (FunIncAMatchCase matchCase : cases)
                        caseTypes.add(typecheckExp(matchCase.getExp(), holder));
                    return FunIncATypeUtil.join(caseTypes);
                } else { // exactly 1 definition of matcheeType was found
                    FunIncADataDef dataDef = dataDefs.get(0);
                    return typecheckTypeNameMatch(exp, cases, dataDef, holder);
                }
            } else if (matcheeType instanceof ConstructorType) { // TODO constructor match
//                PsiElement constrName = PsiTreeUtil.findChildOfType(matchee, FuncIncaVar.class);
//                if (constrName.getReference() != null)
//                    return typecheckConstructorMatch(exp, cases, constrName.getReference().resolve(), matcheeType, holder);

            } else {
                holder.newAnnotation(HighlightSeverity.ERROR,
                                "Cannot match on type " + matcheeType)
                        .range(matchee)
                        .create();
                List<Type> caseTypes = new ArrayList<>();
                for (FunIncAMatchCase matchCase : cases)
                    caseTypes.add(typecheckExp(matchCase.getExp(), holder));
                return FunIncATypeUtil.join(caseTypes);
            }

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
            // TODO FunIncABaseApplyExp
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
            // TODO FunIncABaseApplyMethodExp
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
                case "%":
                case "/":
                case "*":
                case "-":
                    return checkBinArithmeticOp(lhsType, rhsType, op, exp, holder);
                case "&&":
                case ">=":
                case "<=":
                case ">":
                case "<":
                case "||":
                    return checkBinLogicOp(lhsType, rhsType, op, exp, holder);
                case "==":
                case "!=":
                    return new BooleanType();
                case "++":
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
            for (FunIncAExp item : items) {
                Type itemType = typecheckExp(item, holder);
                setTypes.add(itemType);
            }
            Type setType = FunIncATypeUtil.join(setTypes);
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
            FunIncASetComprehensionExp setComprehensionExp = (FunIncASetComprehensionExp) exp;
            int n = setComprehensionExp.getExpList().size();
            if (n == 0) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Incomplete set comprehension")
                        .range(setComprehensionExp)
                        .create();
                return new SetType(new NothingType());
            }
            FunIncAExp build = setComprehensionExp.getExpList().get(0);
            Type buildType = typecheckExp(build, holder);
            if (n == 1) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Missing predicates")
                        .range(setComprehensionExp)
                        .create();
                return new SetType(new NothingType());
            }
            List<FunIncAExp> preds = setComprehensionExp.getExpList().subList(1, setComprehensionExp.getExpList().size());
            for (FunIncAExp pred : preds) {
                Type type = typecheckExp(pred, holder);
                if (! FunIncATypeUtil.subtype(type, new BooleanType())) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                            "Comprehension predicate must have Boolean type, but got " + type)
                            .range(pred)
                            .create();
                }
            }
            return new SetType(buildType);

        } else if (exp instanceof FunIncAFoldExp) {
            FunIncAFoldExp foldExp = (FunIncAFoldExp) exp;
            FunIncAType tyAnno = foldExp.getType();
            FunIncAExp init;
            FunIncAExp op;
            FunIncAExp set;
            try {
                init = foldExp.getExpList().get(0);
                op = foldExp.getExpList().get(1);
                set = foldExp.getExpList().get(2);
            } catch (NullPointerException e) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Incomplete fold expression")
                        .range(foldExp)
                        .create();
                return new AnyType();
            }
            Type initType = typecheckExp(init, holder);
            Type opType = typecheckExp(op, holder);
            Type setType = typecheckExp(set, holder);
            Type setContentType;
            if (setType instanceof SetType) {
                setContentType = ((SetType) setType).getSetType();
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR,
                                "Can only fold over sets, but got " + setType)
                        .range(set)
                        .create();
                setContentType = new NothingType();
            }
            Type foldType;
            if (tyAnno != null) {
                validateType(tyAnno, holder);
                foldType = PsiToTypeConverter.convert(tyAnno);
            } else {
                foldType = FunIncATypeUtil.join(initType, setContentType);
            }
            if (opType instanceof FunType) {
                List<Type> paramTypes = ((FunType) opType).paramTypes;
                Type returnType = ((FunType) opType).returnType;
                if (paramTypes.size() != 2
                        || !FunIncATypeUtil.subtype(paramTypes.get(0), foldType)
                        || !FunIncATypeUtil.subtype(paramTypes.get(1), foldType)
                        || !FunIncATypeUtil.subtype(returnType, foldType)) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                            "Expected function of type (" + foldType + ", " + foldType + ") => " + foldType +
                                    ", but " + op.getText() + " has type " + opType)
                            .range(op)
                            .create();
                }
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Expected function of type (" + foldType + ", " + foldType + ") => " + foldType +
                                ", but " + op.getText() + " has type " + opType)
                        .range(op)
                        .create();
            }
            return foldType;
        }
        return new AnyType();
    }


    private static Type typecheckTypeNameMatch (PsiElement exp,
                                                List<FunIncAMatchCase> cases,
                                                FunIncADataDef dataDef,
                                                AnnotationHolder holder) {
        Set<FunIncADataConstructorDef> seenConstructors = new HashSet<>();
        Set<FunIncADataConstructorDef> availableConstructors = new HashSet<>();
        for (FunIncADataConstructorDef cons : dataDef.getDataConstructorDefList())
            availableConstructors.add(cons);

        List<Type> caseTypes = new ArrayList<>();
        for (FunIncAMatchCase matchCase : cases) {
            PsiElement pat;
            try {
                pat = matchCase.getPat().getFirstChild();
            } catch (NullPointerException e) {
                caseTypes.add(new AnyType());
                continue;
            }
            FunIncAExp e = matchCase.getExp();
            if (pat instanceof FunIncAConstructorPat) {
                FunIncAConstructorPat constructorPat = (FunIncAConstructorPat) pat;
                String constructorName = constructorPat.getConstructorRef().getText();
                FunIncAReference reference = (FunIncAReference) constructorPat.getConstructorRef().getReference();
                FunIncADataConstructorDef constructorDef = null;
                ResolveResult[] result = reference.multiResolve(true);
                for (ResolveResult res : result)
                    if (availableConstructors.contains(res.getElement()))
                        constructorDef = (FunIncADataConstructorDef) res.getElement();
                if (constructorDef == null) { // only if there are 0 or more than 2 results, where none belong to dataDef
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "No constructor " + constructorName + " of type " + dataDef.getName() +
                                            " found")
                            .range(constructorPat.getConstructorRef())
                            .create();
                    caseTypes.add(typecheckExp(e, holder));
                    continue;
                }
                if (!seenConstructors.add(constructorDef)) // this constructor was already used in a match case
                    holder.newAnnotation(HighlightSeverity.ERROR,
                            "Duplicate constructor pattern " + constructorName)
                            .range(constructorPat.getConstructorRef())
                            .create();
                if (constructorDef.getTypeList().size() != constructorPat.getPatternVarDefList().size()) // number of parameters does not match
                    holder.newAnnotation(HighlightSeverity.ERROR,
                            "Wrong number of constructor arguments, expected " +
                                    constructorDef.getTypeList().size() + " but got " +
                                    constructorPat.getPatternVarDefList().size())
                            .range(constructorPat)
                            .create();
                caseTypes.add(typecheckExp(e, holder));
            } else { // pattern is not instance of FuncIncaConstructorPattern
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Cannot match pattern " + pat.getText() +
                        " against matchee of type " + dataDef.getName())
                        .range(pat)
                        .create();

                caseTypes.add(typecheckExp(e, holder));
            }
        }
        availableConstructors.removeAll(seenConstructors);
        if (!availableConstructors.isEmpty()) {
            String missingConstructors = "";
            for (FunIncADataConstructorDef cons : availableConstructors)
                missingConstructors += cons.getName() + ", ";
            holder.newAnnotation(HighlightSeverity.ERROR,
                    "Pattern match must be complete but missed the following constructors: " +
                            missingConstructors.substring(0, missingConstructors.length() - 2))
                    .range(exp)
                    .create();
        }
        return FunIncATypeUtil.join(caseTypes);
    }

    private static Type typecheckConstructorMatch(PsiElement exp,
                                                  List<FunIncAMatchCase> cases,
                                                  FunIncADataDef dataDef,
                                                  AnnotationHolder holder) {
        return new AnyType();
    }
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

    private static Type checkBinLogicOp(Type lhs, Type rhs, String op, PsiElement exp, AnnotationHolder holder) { // >,>=,usw.
        if (op.equals("&&") || op.equals("||")) { // these operators can only be used with Boolean types
            if (lhs.isBooleanType() && rhs.isBooleanType())
                return lhs;
        } else if (op.equals("<") || op.equals(">") || op.equals("<=") || op.equals(">=")){ // these comparators can only compare numeric values
            if (FunIncATypeUtil.subtype(lhs, new DoubleType()) && FunIncATypeUtil.subtype(rhs, new DoubleType()))
                return new BooleanType();
        }
        holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use logic operator " + op +
                        " with types " + lhs + " and " + rhs)
                .range(exp)
                .create();
        return new BooleanType();
    }

    private static Type checkBinSetOp(Type lhs, Type rhs, String op, PsiElement exp, AnnotationHolder holder) {
        String opName = "unknown set operation";
        if (op.equals("++")) opName = "union";
        if (op.equals("&")) opName = "intersection";
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
