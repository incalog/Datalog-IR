package language.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry;
import com.intellij.util.ArrayUtil;
import language.FuncIncaReference;
import language.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;


public class FuncIncaPsiImplUtil {

    private static String getName(ASTNode node){
        if(node != null)
            return node.getText();
        else
            return null;
    }

    private static PsiElement getNameIdentifier(ASTNode node){
        if (node != null) {
            return node.getPsi();
        } else {
            return null;
        }
    }

    public static PsiReference getReference(PsiElement element){
        //PsiReference[] references = ReferenceProvidersRegistry.getReferencesFromProviders(element);
        //PsiReference first = ArrayUtil.getFirstElement(references);
        //return first;
        return new FuncIncaReference(element, element.getTextRange());
    }

    // -------------------------------- var ------------------------------------
    public static String getName(FuncIncaVar element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaVar element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaVar var = FuncIncaElementFactory.createVar(element.getProject(), newName);
            ASTNode newNode = var.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaVar element) {
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // ------------------------------- param -----------------------------------
    public static String getName(FuncIncaParam element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaParam element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaParam paramId = FuncIncaElementFactory.createParam(element.getProject(), newName);
            ASTNode newNode = paramId.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaParam element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // ------------------------------- import ----------------------------------
    public static String getName(FuncIncaImport element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaImport element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaImport imp = FuncIncaElementFactory.createImport(element.getProject(), newName);
            ASTNode newNode = imp.getFirstChild().getNextSibling().getNode(); // first child is keyword import, second child is id
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaImport element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // --------------------------------- fun_def ------------------------------------
    public static String getName(FuncIncaFunDef element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaFunDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaFunDef funDef = FuncIncaElementFactory.createFunDef(element.getProject(), newName);
            ASTNode newNode = funDef.getFirstChild().getNextSibling().getNode(); // first child is keyword_def, next silbling is id
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaFunDef element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // --------------------------------- param_type ---------------------------------

    public static String getName(FuncIncaParamType element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaParamType element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaParamType paramType = FuncIncaElementFactory.createParamType(element.getProject(), newName);
            ASTNode newNode = paramType.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaParamType element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // --------------------------------- data_def -------------------------------------
    public static String getName(FuncIncaDataDef element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaDataDef element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaDataDef dataDef = FuncIncaElementFactory.createDataDef(element.getProject(), newName);
            ASTNode newNode = dataDef.getFirstChild().getNextSibling().getNode(); // first child is keyword_data, next silbling is id
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaDataDef element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // ------------------------------------- data_constructor ------------------------------
    public static String getName(FuncIncaDataConstructor element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaDataConstructor element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaDataConstructor dataCons = FuncIncaElementFactory.createDataConstructor(element.getProject(), newName);
            ASTNode newNode = dataCons.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaDataConstructor element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // ----------------------------- cons_pattern_id ---------------------------------
    public static String getName(FuncIncaConsPatternId element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaConsPatternId element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaConsPatternId cpi = FuncIncaElementFactory.createConsPatternId(element.getProject(), newName);
            ASTNode newNode = cpi.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaConsPatternId element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // ----------------------------- constructor_pattern -----------------------------
    public static String getName(FuncIncaConstructorPattern element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaConstructorPattern element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaConstructorPattern consPattern = FuncIncaElementFactory.createConstructorPattern(element.getProject(), newName);
            ASTNode newNode = consPattern.getFirstChild().getNode(); // first node is the name of the called constructor
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaConstructorPattern element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // -------------------------------- var_id ---------------------------------------
    public static String getName(FuncIncaVarId element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaVarId element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaVarId varId = FuncIncaElementFactory.createVarId(element.getProject(), newName);
            ASTNode newNode = varId.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaVarId element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    //--------------------------------- type_name -------------------------------------

    public static String getName(FuncIncaTypeName element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaTypeName element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaTypeName typeName = FuncIncaElementFactory.createTypeName(element.getProject(), newName);
            ASTNode newNode = typeName.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaTypeName element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }


}
