package language.types;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import language.FuncIncaUtil;
import language.psi.*;
import language.util.*;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;

import static language.types.TypeContext.*;

public class FuncIncaTypechecker {

    public static FuncIncaType typecheck(@NotNull PsiElement exp, @NotNull AnnotationHolder holder) {
        return typecheckCore(exp, holder);
    }
    
    public static FuncIncaType typecheckCore(PsiElement exp, @NotNull AnnotationHolder holder) {
        // System.out.println("AST TYPE " + exp.getClass().getCanonicalName());
        if (exp instanceof FuncIncaVar) {
                String name = exp.getText();
                Pair<FuncIncaDecl, FuncIncaType> var = lookupVar(name, (FuncIncaVar) exp, holder);
                FuncIncaDecl decl = var.getFirst();
                FuncIncaType type = var.getSecond();
                return type;

        } else if (exp instanceof FuncIncaLetExp) {
            FuncIncaSingleLet singleLet = ((FuncIncaLetExp) exp).getSingleLet();
            FuncIncaMultipleLet multLet = ((FuncIncaLetExp) exp).getMultipleLet();
            final FuncIncaType[] bodyType = new FuncIncaType[1];

            // single Let Expression
            if (singleLet != null) {
                FuncIncaDecl decl = singleLet.getVarId();
                String nameText = decl.getText();
                FuncIncaTypeAnnotation typeAnno = singleLet.getTypeAnnotation();
                PsiElement bound = singleLet.getExpList().get(0);
                FuncIncaType boundType = typecheckCore(bound, holder);
                PsiElement body = singleLet.getExpList().get(1);
                if (typeAnno != null) {
                    FuncIncaType expectedType = FuncIncaTypeUtil.psiToFuncIncaType(typeAnno);
                    if (expectedType instanceof FuncIncaUnitType) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                        "Cannot assign Type Unit to " + nameText)
                                .range(typeAnno)
                                .create();
                    }
                    if (!expectedType.equals(boundType)) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                "Expected " + expectedType + ", but got " + boundType)
                                .range(bound)
                                .create();
                    }
                }
                if (boundType.equals(new FuncIncaUnitType())) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                            "Cannot assign expression of Type Unit to " + nameText)
                            .range(typeAnno)
                            .create();
                    //boundType = new FuncIncaAnyType(); // TODO whyyy is this happppeningggg (build ordner löschen? rewrite with bool außerhalb if
                }
                FuncIncaType finalBoundType = boundType;
                scopedTypeContext(new Runnable() {
                    @Override
                    public void run() {
                        bindVar(nameText, decl, finalBoundType, holder);
                        bodyType[0] = typecheckCore(body, holder);
                    }
                });
                return bodyType[0];
            }

            if (multLet != null) {
                List<FuncIncaVarId> decls = multLet.getVarIdList();
                int n = decls.size();
                List<String> nameText = decls.stream().map(e -> e.getText()).collect(Collectors.toList());
                List<FuncIncaTypeAnnotation> typeAnnos = multLet.getTypeAnnotationList();
                PsiElement bound = multLet.getExpList().get(0);
                FuncIncaType boundType = typecheckCore(bound, holder);
                PsiElement body = multLet.getExpList().get(1);
                if (typeAnnos != null) {
                    // TODO
                }
                if (!(boundType instanceof FuncIncaTupleType)) {
                    holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Expected Tuple type, but got " + boundType)
                            .range(bound)
                            .create();
                    // boundType = new FuncIncaTupleType(Collections.nCopies(n, new FuncIncaAnyType()));
                } else {
                    List<FuncIncaType> tupleTypes = ((FuncIncaTupleType) boundType).getTypes();
                    int m = tupleTypes.size();
                    if (n != m) {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                "Cannot assign " + m + "-ary tuple to " + n + " variables")
                                .range(bound)
                                .create();
                        if (n < m) { // more types than variables
                            // boundType = new FuncIncaTupleType(tupleTypes.subList(0, n));
                        } else { // n > m, more variables than types
                            List<FuncIncaType> additionalTypes = Collections.nCopies(n-m, new FuncIncaAnyType());
                            tupleTypes.addAll(additionalTypes);
                            // boundType = new FuncIncaTupleType(tupleTypes);
                        }
                    }
                }
                List<FuncIncaType> tupleTypes = ((FuncIncaTupleType) boundType).getTypes();
                scopedTypeContext(new Runnable() {
                    @Override
                    public void run() {
                        for (int i = 0; i < n; i++) {
                            bindVar(nameText.get(i), decls.get(i), tupleTypes.get(i), holder);
                        }
                        bodyType[0] = typecheckCore(body, holder);
                    }
                });
                return bodyType[0];
            }
        } else if (exp instanceof FuncIncaCastExp) {
            PsiElement ex = ((FuncIncaCastExp) exp).getExp();
            PsiElement ct = ((FuncIncaCastExp) exp).getTypeName();
            FuncIncaType castType = new FuncIncaTypeNameType(ct.getText());
            FuncIncaType exType = typecheckCore(ex, holder);
            if (meet(exType, castType).equals(new FuncIncaNothingType()))
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Type cast of " + castType + " is not compatible with inferred type " + exType + " of e " + exp).
                        range(exp.getTextRange()).create();
            return castType;
        } else if (exp instanceof FuncIncaIfExp) {
            PsiElement cond = ((FuncIncaIfExp) exp).getExpList().get(0);
            FuncIncaType condType = typecheck(cond, holder);
            if (!(condType instanceof FuncIncaBooleanType)) {
                holder.newAnnotation(HighlightSeverity.ERROR,
                        "Expected Boolean condition, but got " + condType)
                        .range(cond)
                        .create();
            }
            FuncIncaType thenType = typecheck(((FuncIncaIfExp) exp).getExpList().get(1), holder);
            FuncIncaType elseType = typecheck(((FuncIncaIfExp) exp).getExpList().get(2), holder);
            return join(thenType, elseType);
        } else if (exp instanceof FuncIncaParensExp) {
            return typecheckCore(((FuncIncaParensExp) exp).getExp(), holder);
        } else if (exp instanceof FuncIncaTupleExp) {
            List<FuncIncaType> tupleTypes = new ArrayList<>();
            for (PsiElement e : ((FuncIncaTupleExp) exp).getExpList())
                tupleTypes.add(typecheck(e, holder));
            return new FuncIncaTupleType(tupleTypes);
        } else if (exp instanceof FuncIncaLambdaExp) {
            List<FuncIncaParam> params = ((FuncIncaLambdaExp) exp).getParamList().getParamList();
            FuncIncaExp body = ((FuncIncaLambdaExp) exp).getExp();
            final FuncIncaType[] returnType = new FuncIncaType[1];
            scopedTypeContext(new Runnable() {
                @Override
                public void run() {
                    for (FuncIncaParam param : params) {
                        String name = param.getText();
                        FuncIncaType type = FuncIncaTypeUtil.psiToFuncIncaType(param.getTypeAnnotation());
                        bindVar(name, param, type, holder);
                    }
                    returnType[0] = typecheckCore(body, holder);
                }
            });
            return returnType[0];

        } else if (exp instanceof FuncIncaCallExp) {

        } else if (exp instanceof FuncIncaMatchExp) {
            FuncIncaExp matchee = ((FuncIncaMatchExp) exp).getExp();
            List<FuncIncaMatchCase> cases = ((FuncIncaMatchExp) exp).getMatchCaseList();
            FuncIncaType matcheeType = typecheck(matchee, holder);
            if (matcheeType instanceof FuncIncaTypeNameType) {
                PsiElement tName = PsiTreeUtil.getChildOfType(matchee, FuncIncaVar.class);
                if (tName.getReference() != null) { // tName is already defined
                    FuncIncaType tNameType = typecheckTypeNameMatch(exp, cases, tName.getReference().resolve(), holder);
                    if (tNameType instanceof FuncIncaParameterizedType){
                        holder.newAnnotation(HighlightSeverity.ERROR, "Cannot match on parametric type " + tNameType).
                                range(matchee.getTextRange()).create();
                        List<FuncIncaType> caseTypes = new ArrayList<>();
                        for (FuncIncaMatchCase matchCase : cases)
                            caseTypes.add(typecheck(matchCase.getExp(), holder));
                        return join(caseTypes);
                    }
                    return tNameType;
                }
            } else if (matcheeType instanceof FuncIncaConstructorType) {
                PsiElement constrName = PsiTreeUtil.findChildOfType(matchee, FuncIncaVar.class); // TODO find correct definition of the datatype
                if (constrName.getReference() != null)
                    return typecheckConstructorMatch(exp, cases, constrName.getReference().resolve(), matcheeType, holder);
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot match on type " + matcheeType).
                        range(matchee.getTextRange()).create();
                List<FuncIncaType> caseTypes = new ArrayList<>();
                for (FuncIncaMatchCase matchCase : cases)
                    caseTypes.add(typecheck(matchCase.getExp(), holder));
                return join(caseTypes);
            }
        } else if (exp instanceof FuncIncaBaseLitExp) {
            PsiElement e = exp.getFirstChild();
            if (e instanceof FuncIncaIntLit) {
                return new FuncIncaIntegerType();
            } else if (e instanceof FuncIncaLongLit) {
                return new FuncIncaLongType();
            } else if (e instanceof FuncIncaDoubleLit) {
                return new FuncIncaDoubleType();
            } else if (e instanceof FuncIncaBooleanLit) {
                return new FuncIncaBooleanType();
            } else if (e instanceof FuncIncaStringLit) {
                return new FuncIncaStringType();
            } else {
                // type scalaterm
            }
        } else
            if (exp instanceof FuncIncaBaseApplyExp) {

        } else if (exp instanceof FuncIncaBaseApplyUnaryExp) {
            String op = ((FuncIncaBaseApplyUnaryExp) exp).getUnaryOp().getText();
            FuncIncaType eType = typecheckCore(((FuncIncaBaseApplyUnaryExp) exp).getExp(), holder);
            switch (op) {
                case "-":
                    if (eType.isIntType() || eType.isDoubleType() || eType.isLongType()) {
                        return eType;
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR, "Arithetic operator - cannot be used" +
                                        " with type " + eType + ".")
                                .range(exp)
                                .create();
                        return new FuncIncaAnyType();
                    }
                case "!":
                    if (eType.isBooleanType()) {
                        return eType;
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR, "Logic operator ! cannot be used" +
                                        " with type " + eType + ".")
                                .range(exp)
                                .create();
                        return new FuncIncaAnyType();
                    }
                default:
                    holder.newAnnotation(HighlightSeverity.ERROR, "Operation " + op + " is not supported.")
                            .range(exp)
                            .create();
                    return new FuncIncaAnyType();

            }
        } else
            if (exp instanceof FuncIncaBaseApplyMethodExp) {

        } else
            if (exp instanceof FuncIncaBaseApplyInfixExp) {
            List<FuncIncaExp> children = ((FuncIncaBaseApplyInfixExp) exp).getExpList();
            String op = ((FuncIncaBaseApplyInfixExp) exp).getOp().getText();
            FuncIncaType lhsType = typecheckCore(children.get(0), holder);
            if (children.size() == 1)
                return lhsType;
            FuncIncaType rhsType = typecheckCore(children.get(1), holder);
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
                    return new FuncIncaAnyType();
            }

        } else if (exp instanceof FuncIncaConstSetExp) {
            List<FuncIncaExp> items = ((FuncIncaConstSetExp) exp).getExpList(); // TODO join ItemTypes to determine SetType
            List<FuncIncaType> setTypes = new ArrayList<>();
            FuncIncaType setType = new FuncIncaNothingType();
            if (items.size() != 0) {
                for (FuncIncaExp item : items) {
                    FuncIncaType itemType = typecheckCore(item, holder);
                    setTypes.add(itemType);
                }
                setType = join(setTypes);
            }
            return new FuncIncaSetType(setType);
        } else if (exp instanceof FuncIncaMemberExp) {

        } else if (exp instanceof FuncIncaComprehensionExp) {

        } else if (exp instanceof FuncIncaFoldExp) {

        }
        // TODO
        return new FuncIncaAnyType();
    }
    
    public static Boolean subtype(FuncIncaType subType, FuncIncaType superType) {
        return (meet(subType, superType)).equals(subType);
    }

    private static FuncIncaType meet(List<FuncIncaType> types) {
        return types.stream().reduce(new FuncIncaAnyType(), (a, b) -> meet(a, b));
    }

    private static FuncIncaType meet(FuncIncaType type1, FuncIncaType type2){
        if (type1.equals(type2)) {
            return type1;
        } else if(type1 instanceof FuncIncaAnyType){
            return type2;
        } else if (type2 instanceof FuncIncaAnyType) {
            return type1;
        } else if (type1 instanceof FuncIncaTypeNameType && type2 instanceof FuncIncaConstructorType) {
            if(((FuncIncaTypeNameType) type1).getName().equals(((FuncIncaConstructorType) type2).getName()))
                return type1;
        } else if (type1 instanceof FuncIncaConstructorType && type2 instanceof FuncIncaTypeNameType) {
            if(((FuncIncaConstructorType) type1).getName().equals(((FuncIncaTypeNameType) type2).getName()))
                return type1;
        } else if (type1 instanceof FuncIncaTupleType && type2 instanceof FuncIncaTupleType) {
            List<FuncIncaType> tupleTypes1 = ((FuncIncaTupleType) type1).getTypes();
            List<FuncIncaType> tupleTypes2 = ((FuncIncaTupleType) type2).getTypes();
            if(tupleTypes1.size() == tupleTypes2.size()){
                int n = tupleTypes1.size();
                List<FuncIncaType> tupTys = new ArrayList<>(n);
                for (int i = 0; i < n; i++)
                    tupTys.add(meet(tupleTypes1.get(i), tupleTypes2.get(i)));
                return new FuncIncaTupleType(tupTys);
            }
        } else if (type1 instanceof FuncIncaSetType && type2 instanceof FuncIncaSetType) {
            FuncIncaType setType1 = ((FuncIncaSetType) type1).getSetType();
            FuncIncaType setType2 = ((FuncIncaSetType) type2).getSetType();
            return new FuncIncaSetType(meet(setType1, setType2));
        } else if (type1 instanceof FuncIncaBooleanType) { // meet of primitive types
            // case of type2 is Any or Boolean already covered, Nothing Type is returned further down
        } else if (type1 instanceof FuncIncaDoubleType) {
            if (type2 instanceof FuncIncaLongType || type2 instanceof FuncIncaIntegerType) {
                return type2;
            }
        } else if (type1 instanceof FuncIncaIntegerType) {
            if (type2 instanceof FuncIncaDoubleType || type2 instanceof FuncIncaLongType) {
                return new FuncIncaIntegerType();
            }
        } else if (type1 instanceof FuncIncaLongType) {
            if (type2 instanceof FuncIncaDoubleType) {
                return new FuncIncaLongType();
            } else if (type2 instanceof FuncIncaIntegerType) {
                return new FuncIncaIntegerType();
            }
        } else if (type1 instanceof FuncIncaStringType) {

        }
        return new FuncIncaNothingType();
    }

    private static FuncIncaType join(List<FuncIncaType> types) {
        return types.stream().reduce(new FuncIncaNothingType(), (a, b) -> join(a, b));
    }

    private static FuncIncaType join(FuncIncaType type1, FuncIncaType type2) {
        if(type1.equals(type2)){
            return type1;
        } else if (type1 instanceof FuncIncaNothingType){
            return type2;
        } else if (type2 instanceof FuncIncaNothingType) {
            return type1;
        } else if (type1 instanceof FuncIncaTupleType && type2 instanceof FuncIncaTupleType) {
            List<FuncIncaType> tupleTypes1 = ((FuncIncaTupleType) type1).getTypes();
            List<FuncIncaType> tupleTypes2 = ((FuncIncaTupleType) type2).getTypes();
            if(tupleTypes1.size() == tupleTypes2.size()){
                int n = tupleTypes1.size();
                List<FuncIncaType> tupTys = new ArrayList<>(n);
                for (int i = 0; i < n; i++)
                    tupTys.add(join(tupleTypes1.get(i), tupleTypes2.get(i)));
                return new FuncIncaTupleType(tupTys);
            }
        } else if (type1 instanceof FuncIncaSetType && type2 instanceof FuncIncaSetType) {
            FuncIncaType setType1 = ((FuncIncaSetType) type1).getSetType();
            FuncIncaType setType2 = ((FuncIncaSetType) type2).getSetType();
            return new FuncIncaSetType(join(setType1, setType2));
        } else if (type1 instanceof FuncIncaBooleanType) { // join of primitive scalatypes
            // case type2 is Nothing or Boolean is already covered, return of Any is down below
        } else if (type1 instanceof FuncIncaDoubleType) {
            if (type2 instanceof FuncIncaLongType || type2 instanceof FuncIncaIntegerType) {
                return new FuncIncaDoubleType();
            }
        } else if (type1 instanceof FuncIncaIntegerType) {
            if (type2 instanceof FuncIncaLongType || type2 instanceof FuncIncaDoubleType) {
                return type2;
            }
        } else if (type1 instanceof FuncIncaLongType) {
            if (type2 instanceof FuncIncaIntegerType) {
                return type1;
            } else if (type2 instanceof FuncIncaDoubleType) {
                return type2;
            }
        } else if (type1 instanceof FuncIncaStringType) {

        }
        return new FuncIncaAnyType();
    }

    private static FuncIncaType typecheckTypeNameMatch(PsiElement exp,
                                                       List<FuncIncaMatchCase> cases,
                                                       PsiElement tName, // the part of PSI tree, where this data got defined
                                                       AnnotationHolder holder) {

        Map<String, Integer> seenConstr = new HashMap<>();
        Map<String, Integer> availableConstr = new HashMap<>();
        for (FuncIncaDataConstructor c : ((FuncIncaDataDef) tName).getDataConstructorList())
            availableConstr.put(c.getId().getText(), c.getTypeAnnotationList().size());

        List<FuncIncaType> caseTypes = new ArrayList<>();
        for (FuncIncaMatchCase matchCase : cases){
            FuncIncaPattern pattern = matchCase.getPattern();
            PsiElement e = matchCase.getExp();
            if (pattern instanceof FuncIncaConstructorPattern) {
                int params = ((FuncIncaConstructorPattern) pattern).getConsPatternIdList().size();
                FuncIncaConsId consId = ((FuncIncaConstructorPattern) pattern).getConsId();
                if (seenConstr.containsKey(consId.getText()))
                    holder.newAnnotation(HighlightSeverity.ERROR, "Duplicate constructor pattern " + consId).
                            range(consId.getTextRange()).create();
                else
                    seenConstr.put(consId.getText(), params);

                for (Map.Entry<String, Integer> ac : availableConstr.entrySet()){
                    if (seenConstr.containsKey(ac.getKey())) {
                        if (params != ac.getValue())
                            holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Wrong number of constructor arguments, expected " + ac.getValue() +
                                            " but got " + params).range(pattern).create();
                        // TODO scopedTypeContext {
                        //              vars.zipAll(paramTypes, null, null).foreach {
                        //                case (null, ty) => // nothing
                        //                case (v, null) => bindVar(v.name, pat, TAny)
                        //                case (v, ty) => bindVar(v.name, pat, ty)
                        //              }
                        //              typecheck(e)
                        //            }
                        caseTypes.add(typecheck(e, holder));
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                "Cannot match constructor " + consId + " against matchee of type " +
                                        ((FuncIncaDataDef) tName).getId()).range(consId).create();
                        // TODO scopedTypeContext {
                        //              vars.foreach(v => bindVar(v.name, pat, TAny))
                        //              typecheck(e)
                        //            }
                        caseTypes.add(typecheck(e, holder));
                    }
                }
            } else { // pattern is not instance of FuncIncaConstructorPattern
                holder.newAnnotation(HighlightSeverity.ERROR,"Cannot match pattern " + pattern +
                        " against matchee of type " + ((FuncIncaDataDef) tName).getId()).range(pattern).create();
                // TODO scopedTypeContext {
                //          val dummy = ConstructorPattern(Name("?"), Seq())
                //          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
                //          typecheck(e)
                //        }
                caseTypes.add(typecheck(e, holder));
            }
        }

        Set<String> missingCons = new HashSet<>();
        for (Map.Entry<String, Integer> ac : availableConstr.entrySet())
            if (!seenConstr.containsKey(ac.getKey()))
                missingCons.add(ac.getKey());
        holder.newAnnotation(HighlightSeverity.ERROR,
                "Pattern must be complete but missed the following constructors: " + missingCons)
                .range(exp).create();
        return join(caseTypes);
    }

    private static FuncIncaType typecheckConstructorMatch(PsiElement exp, // matchee is a FuncIncaConstructorType
                                                          List<FuncIncaMatchCase> cases,
                                                          PsiElement consName, // definition of this TypeConstructor matchee
                                                          FuncIncaType matcheeType, // seperate, bc types do not reference their definition
                                                          AnnotationHolder holder) {
        PsiElement data = consName;
        Map<String, DataConstructor> seenConstr = new HashMap<>();
        Map<String, DataConstructor> availableConstr = new HashMap<>();
        Map<String, FuncIncaType> subst = new HashMap<>(); // maps name of parameterized type to the currently used type

        for (int i = 0; i < ((FuncIncaConstructorType) matcheeType).getTypes().size(); i++) {
            FuncIncaType ty = ((FuncIncaConstructorType) matcheeType).getTypes().get(i);
            String param = ((FuncIncaDataDef) consName).getParamTypes().getParamTypeList().get(i).getText();
            subst.put(param, ty);
        }

        for (FuncIncaDataConstructor c : ((FuncIncaDataDef) data).getDataConstructorList()) {
            List<FuncIncaType> tyVars = new ArrayList<>();
            for (FuncIncaParamType paramTy : c.getParamTypes().getParamTypeList())
                tyVars.add(subst.get(paramTy.getText()));
            availableConstr.put(c.getId().getText(),
                    new DataConstructor(c.getId().getText(),
                            tyVars,
                            FuncIncaTypeUtil.psiToFuncIncaType(c.getTypeAnnotationList())));
        }

        List<FuncIncaType> caseTypes = new ArrayList<>();
        for (FuncIncaMatchCase matchCase : cases) {
            FuncIncaPattern pattern = matchCase.getPattern();
            PsiElement e = matchCase.getExp();
            if (pattern instanceof FuncIncaConstructorPattern) {
                int params = ((FuncIncaConstructorPattern) pattern).getConsPatternIdList().size(); // number of parameters in patternmatch
                FuncIncaConsId consId = ((FuncIncaConstructorPattern) pattern).getConsId();
                if (seenConstr.containsKey(consId.getText()))
                    holder.newAnnotation(HighlightSeverity.ERROR, "Duplicate constructor pattern " + consId).
                            range(consId.getTextRange()).create();
                else
                    seenConstr.put(consId.getText(), new DataConstructor(consId.getText(),
                            FuncIncaTypeUtil.paramTypeToFuncIncaType(((FuncIncaConstructorPattern) pattern).getParamTypes().getParamTypeList()),
                            Collections.nCopies(params, new FuncIncaAnyType())));

                // TODO line 466
                for (Map.Entry<String, DataConstructor> ac : availableConstr.entrySet()) {
                    if (seenConstr.containsKey(ac.getKey())) {
                        if (params != ac.getValue().getTypes().size())
                            holder.newAnnotation(HighlightSeverity.ERROR,
                                    "Wrong number of constructor aruments, expected " + ac.getValue().getTypes().size()
                            + " but got " + params).range(pattern).create();
                        // TODO scopedTypeContext {
                        //              vars.zipAll(paramTypes, null, null).foreach {
                        //                case (null, ty) => // nothing
                        //                case (v, null) => bindVar(v.name, pat, TAny)
                        //                case (v, ty) => bindVar(v.name, pat, ty)
                        //              }
                        //              typecheck(e)
                        //            }
                        caseTypes.add(typecheck(e, holder));
                    } else {
                        holder.newAnnotation(HighlightSeverity.ERROR,
                                "Cannot match constructor " + consId + " against matchee of type " + matcheeType)
                                .range(pattern)
                                .create();
                        // TODO scopedTypeContext {
                        //              vars.foreach(v => bindVar(v.name, pat, TAny))
                        //              typecheck(e)
                        //            }
                    }
                }
            } else { // pattern is not instance of FuncIncaConstructorPattern
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot match pattern " + pattern
                        + " against type " + consName).range(pattern).create();
                // TODO scopedTypeContext {
                //          val dummy = ConstructorPattern(Name("?"), Seq())
                //          pat.vars.foreach(v => bindVar(v._1, dummy, TAny))
                //          typecheck(e)
                //        }
                caseTypes.add(typecheck(e, holder));
            }
        }
        Set<String> missingCons = new HashSet<>();
        for (Map.Entry<String, DataConstructor> ac : availableConstr.entrySet())
            if (!seenConstr.containsKey(ac.getKey()))
                missingCons.add(ac.getKey());
        holder.newAnnotation(HighlightSeverity.ERROR,
                        "Pattern must be complete but missed the following constructors: " + missingCons)
                .range(exp).create();
        return join(caseTypes);
    }


    private static FuncIncaType checkBinArithmeticOp(FuncIncaType lhs,
                                                  FuncIncaType rhs,
                                                  String op,
                                                  PsiElement exp,
                                                  AnnotationHolder holder) {
        if (lhs.isIntType()) {
            if (rhs.isIntType() || rhs.isDoubleType() || rhs.isLongType()) {
                return rhs;
            } else { // rhs not a numeric type
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                                " with types " + lhs + " and " + rhs)
                        .range(exp)
                        .create();
                return new FuncIncaAnyType();
            }
        } else if (lhs.isDoubleType()) {
            if (rhs.isIntType() || rhs.isDoubleType()) {
                return lhs;
            } else if (rhs.isLongType()) {
                // TODO double + long = ???
                return new FuncIncaAnyType();
            } else { // rhs not a numeric type
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                                " with types " + lhs + " and " + rhs)
                        .range(exp)
                        .create();
                return new FuncIncaAnyType();
            }
        } else if (lhs.isLongType()) {
            if (rhs.isIntType() || rhs.isLongType()) {
                return lhs;
            } else if (rhs.isDoubleType()) {
                // TODO long + double = ???
                return new FuncIncaAnyType();
            } else { // rhs is not a numeric type
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                                " with types " + lhs + " and " + rhs)
                        .range(exp)
                        .create();
                return new FuncIncaAnyType();
            }
        } else { // lhs not a numeric type
            holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use arithmetic operator " + op +
                            " with types " + lhs + " and " + rhs)
                    .range(exp)
                    .create();
            return new FuncIncaAnyType();
        }
    }

    private static FuncIncaType checkBinLogicOp(FuncIncaType lhs,
                                             FuncIncaType rhs,
                                             String op,
                                             PsiElement exp,
                                             AnnotationHolder holder) {
        if (lhs.isBooleanType() && rhs.isBooleanType())
            return lhs;
        else {
            holder.newAnnotation(HighlightSeverity.ERROR, "Cannot use logic operator " + op +
                            " with types " + lhs + " and " + rhs)
                    .range(exp)
                    .create();
            return new FuncIncaAnyType();
        }
    }

    private static FuncIncaType checkBinSetOp(FuncIncaType lhs,
                                              FuncIncaType rhs,
                                              String op,
                                              PsiElement exp,
                                              AnnotationHolder holder) {
        String opName = "unknown set operation";
        if (op.equals("++")) opName = "union";
        if (op.equals("&")) opName = "intersetion";
        if (lhs.isSetType() && rhs.isSetType()){
            FuncIncaType lhsSetType = ((FuncIncaSetType) lhs).getSetType();
            FuncIncaType rhsSetType = ((FuncIncaSetType) rhs).getSetType();
            if (lhsSetType.equals(rhsSetType)) {
                return lhs;
            } else {
                holder.newAnnotation(HighlightSeverity.ERROR, "Cannot perform " + opName + " on sets holding " +
                                "different types: " + lhsSetType + " and " + rhsSetType + ".")
                        .range(exp)
                        .create();
                return new FuncIncaAnyType();
            }
        } else {
            holder.newAnnotation(HighlightSeverity.ERROR, "Cannot perform " + opName + " on non set types: "
                            + lhs + " and " + rhs + ".")
                    .range(exp)
                    .create();
            return new FuncIncaAnyType();
        }
    }

}
