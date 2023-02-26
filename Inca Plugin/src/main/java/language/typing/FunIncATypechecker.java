package language.typing;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveResult;
import language.FunIncAReference;
import language.psi.*;
import language.typing.types.*;
import org.jetbrains.annotations.NotNull;

import java.util.*;

// jimport static language.typing.TypeContext;

public class FunIncATypechecker {

    public static Type typecheck(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
        if (element instanceof FunIncAFunDef) {
            FunIncAFunDef funDef = (FunIncAFunDef) element;
        } else if (element instanceof FunIncATypeVarDef){
            // fundef or datadef
        } else if (element instanceof FunIncAParamDef) {
            FunIncAParamDef paramDef = (FunIncAParamDef) element;
            return PsiToTypeConverter.convert(paramDef.getType());
        } else if (element instanceof FunIncADataDef) {
            FunIncADataDef dataDef = (FunIncADataDef) element;
            // return PsiToTypeConverter.convert(dataDef.getType());
        } else if (element instanceof FunIncADataConstructorDef) {
        } else if (element instanceof FunIncAVarDef) {
            FunIncAVarDef varDef = (FunIncAVarDef) element;
            PsiElement parent = varDef.getParent();
            if (parent instanceof FunIncASingleBinding) {
                FunIncASingleBinding binding = (FunIncASingleBinding) parent;
                if (binding.getType() == null) {
                    return typecheckCore(binding.getExpList().get(0), holder);
                } else {
                    Type inferredType = typecheckCore(binding.getExpList().get(0), holder);
                    Type expectedType = PsiToTypeConverter.convert(binding.getType());
                    if (expectedType instanceof UnitType) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Cannot assign Type Unit to " + binding.getVarDef().getName())
                                .range(binding.getType())
                                .create();
                    }
                    if (!FunIncATypeUtil.subtype(inferredType, expectedType)) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Expected " + expectedType + ", but got " + inferredType)
                                .range(binding.getType())
                                .create();
                    }
                    return expectedType;
                }
            } else if (parent instanceof  FunIncAMultipleBindings) {
            } else {
            }
        } else if (element instanceof FunIncAExp) {
            return typecheckCore((FunIncAExp) element, holder);
        }
        return null;
    }

    public static Type typecheckCore(FunIncAExp exp, @NotNull AnnotationHolder holder) {
        if (exp instanceof FunIncAVarRefExp) {
            FunIncAVarRefExp varExp = (FunIncAVarRefExp)  exp;
            FunIncAReference reference = (FunIncAReference) varExp.getReference();
            ResolveResult[] result = reference.multiResolve(true);
            if (result.length == 0) {
                holder.newAnnotation(HighlightSeverity.ERROR, "Unresolved name " + varExp.getName())
                        .range(varExp)
                        .create();
                return new AnyType();
            } else if (result.length == 1){
                if (result[0].isValidResult()) {
                    Type resolvedType = typecheck(result[0].getElement(), holder);
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
                FunIncAExp body = single.getExpList().get(single.getExpList().size() - 1);
                return typecheckCore(body, holder);
            } else if (letExp.getMultipleBindings() != null) {
                FunIncAMultipleBindings multiple = letExp.getMultipleBindings();
                FunIncAExp body = multiple.getExpList().get(multiple.getExpList().size() - 1);
                return typecheckCore(body, holder);
            } else {
                return new AnyType();
            }
//            if (multLet != null) {
//                List<FuncIncaVarId> decls = multLet.getVarIdList();
//                int n = decls.size();
//                List<String> nameText = decls.stream().map(e -> e.getText()).collect(Collectors.toList());
//                List<FuncIncaTypeAnnotation> typeAnnos = multLet.getTypeAnnotationList();
//                PsiElement bound = multLet.getExpList().get(0);
//                FuncIncaType boundType = typecheckCore(bound, holder);
//                PsiElement body = multLet.getExpList().get(1);
//                if (typeAnnos != null) {
//                    // TODO
//                }
//                if (!(boundType instanceof FuncIncaTupleType)) {
//                    holder.newAnnotation(HighlightSeverity.ERROR,
//                                    "Expected Tuple type, but got " + boundType)
//                            .range(bound)
//                            .create();
//                    boundType = new FuncIncaTupleType(Collections.nCopies(n, new FuncIncaAnyType()));
//                } else {
//                    List<FuncIncaType> tupleTypes = ((FuncIncaTupleType) boundType).getTypes();
//                    int m = tupleTypes.size();
//                    if (n != m) {
//                        holder.newAnnotation(HighlightSeverity.ERROR,
//                                "Cannot assign " + m + "-ary tuple to " + n + " variables")
//                                .range(bound)
//                                .create();
//                        if (n < m) { // more types than variables
//                            boundType = new FuncIncaTupleType(tupleTypes.subList(0, n));
//                        } else { // n > m, more variables than types
//                            List<FuncIncaType> additionalTypes = Collections.nCopies(n-m, new FuncIncaAnyType());
//                            tupleTypes.addAll(additionalTypes);
//                            boundType = new FuncIncaTupleType(tupleTypes);
//                        }
//                    }
//                }
//                List<FuncIncaType> tupleTypes = ((FuncIncaTupleType) boundType).getTypes();
//                scopedTypeContext(new Runnable() {
//                    @Override
//                    public void run() {
//                        for (int i = 0; i < n; i++) {
//                            bindVar(nameText.get(i), decls.get(i), tupleTypes.get(i), holder);
//                        }
//                        bodyType[0] = typecheckCore(body, holder);
//                    }
//                });
//                return bodyType[0];
//            }
        } else if (exp instanceof FunIncACastExp) {
            FunIncACastExp cast = (FunIncACastExp) exp;
            FunIncAExp ex = cast.getExp();
            Type expectedType = new TypeRef(Objects.requireNonNull(cast.getTypeNameRef()).getText());
            Type inferredType = typecheckCore(ex, holder);
            if (FunIncATypeUtil.meet(expectedType, inferredType).equals(new NothingType()))
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Type cast of " + expectedType + " is not compatible with inferred type " + inferredType + " of e " + exp).
                        range(exp.getTextRange()).create();
            return expectedType;
        } else if (exp instanceof FunIncAIfExp) {
            FunIncAIfExp ifExp = (FunIncAIfExp) exp;
            PsiElement cond = ifExp.getExpList().get(0);
            Type condType = typecheck(cond, holder);
            if (!(condType instanceof BooleanType)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Expected Boolean condition, but got " + condType)
                        .range(cond)
                        .create();
            }
            Type thenType = typecheck(ifExp.getExpList().get(1), holder);
            Type elseType = typecheck(ifExp.getExpList().get(2), holder);
            return FunIncATypeUtil.join(thenType, elseType);
        } else if (exp instanceof FunIncAParenthesisExp) {
            return typecheckCore(((FunIncAParenthesisExp) exp).getExp(), holder);
        } else if (exp instanceof FunIncATupleExp) {
            List<Type> tupleTypes = new ArrayList<>();
            for (PsiElement e : ((FunIncATupleExp) exp).getExpList())
                tupleTypes.add(typecheck(e, holder));
            return new TupleType(tupleTypes);
        } else if (exp instanceof FunIncALambdaExp) {
//            List<FuncIncaParam> params = ((FuncIncaLambdaExp) exp).getParamList().getParamList();
//            FuncIncaExp body = ((FuncIncaLambdaExp) exp).getExp();
//            List<FuncIncaType> types = new ArrayList<>();
//            final FuncIncaType[] returnType = new FuncIncaType[1];
//            scopedTypeContext(new Runnable() {
//                @Override
//                public void run() {
//                    for (FuncIncaParam param : params) {
//                        String name = param.getId().getText();
//                        FuncIncaType type = FuncIncaTypeUtil.psiToFuncIncaType(param.getTypeAnnotation());
//                        types.add(type);
//                        bindVar(name, param, type, holder);
//                    }
//                    returnType[0] = typecheckCore(body, holder);
//                }
//            });
//            return new FuncIncaFunctionType(new ArrayList<>(), types, returnType[0]);
//
        } else if (exp instanceof FunIncACallExp) {
            // TODO?

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
            Type eType = typecheckCore(unaryExp.getExp(), holder);
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

        } else if (exp instanceof FunIncABaseApplyInfixExp) {
            FunIncABaseApplyInfixExp infixExp = (FunIncABaseApplyInfixExp) exp;
            List<FunIncAExp> children = infixExp.getExpList();
            String op = infixExp.getBinaryOp().getText();
            Type lhsType = typecheckCore(children.get(0), holder);
            if (children.size() == 1)
                return lhsType;
            Type rhsType = typecheckCore(children.get(1), holder);
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
                    Type itemType = typecheckCore(item, holder);
                    setTypes.add(itemType);
                }
                setType = FunIncATypeUtil.join(setTypes);
            }
            return new SetType(setType);
        } else if (exp instanceof FunIncASetMemberExp) {

        } else if (exp instanceof FunIncASetComprehensionExp) {

        } else if (exp instanceof FunIncAFoldExp) {
        }
        // TODO
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
