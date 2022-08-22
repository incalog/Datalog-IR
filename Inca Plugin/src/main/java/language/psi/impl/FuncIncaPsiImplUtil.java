package language.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
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

    // ------------------------------- var ----------------------------------
    public static String getName(FuncIncaVar element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaVar element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaVar var = FuncIncaElementFactory.createVar(element.getProject(), newName); // element factory is not yet implemented
            ASTNode newNode = var.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaVar element){
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

    // --------------------------------- param_types ---------------------------------

    public static List<String> getName(FuncIncaParamTypes element){
        List<String> res = new ArrayList<>();
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID); // first id of multiple
        res.add(getName(idNode));
        do{
            idNode = idNode.getTreeNext(); // checks type of every child of the multiple let construct
            if(idNode.getElementType() == FuncIncaTypes.ID)
                res.add(getName(idNode));
        } while(!(idNode.getElementType() == FuncIncaTypes.SQUARE_BRACKET_CLOSE)); // loop ends when it finds closing parenthesis
        return res;
    }

    public static PsiElement setName(FuncIncaParamTypes element, List<String> newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaParamTypes paramTypes = FuncIncaElementFactory.createParamTypes(element.getProject(), newName);
            do{
                if(idNode != null && idNode.getElementType() == FuncIncaTypes.ID){
                    // TODO replacing the relevant ID Children from the PSI Element element with the new names from paramTypes
                }
                idNode = idNode.getTreeNext();
            } while(idNode.getElementType() != FuncIncaTypes.SQUARE_BRACKET_CLOSE);
        }
        return element;
    }

    public static List<PsiElement> getNameIdentifier(FuncIncaParamTypes element){
        List<PsiElement> res = new ArrayList<>();
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID); // first id of multiple
        res.add(getNameIdentifier(idNode));
        do{
            idNode = idNode.getTreeNext();  // checks type of every child of the multiple let construct
            if(idNode.getElementType() == FuncIncaTypes.ID)
                res.add(getNameIdentifier(idNode));
        } while(!(idNode.getElementType() == FuncIncaTypes.SQUARE_BRACKET_CLOSE)); // loop ends when it finds closing parenthesis
        return res;
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

    // -------------------------------- single_let ---------------------------------------
    public static String getName(FuncIncaSingleLet element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getName(idNode);
    }

    public static PsiElement setName(FuncIncaSingleLet element, String newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaSingleLet singleLet = FuncIncaElementFactory.createSingleLet(element.getProject(), newName);
            ASTNode newNode = singleLet.getFirstChild().getNode();
            element.getNode().replaceChild(idNode, newNode);
        }
        return element;
    }

    public static PsiElement getNameIdentifier(FuncIncaSingleLet element){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        return getNameIdentifier(idNode);
    }

    // -------------------------------------- multiple_let ---------------------------------
    public static List<String> getName(FuncIncaMultipleLet element){
        List<String> res = new ArrayList<>();
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID); // first id of multiple
        res.add(getName(idNode));
        do{
            idNode = idNode.getTreeNext(); // checks type of every child of the multiple let construct
            if(idNode.getElementType() == FuncIncaTypes.ID)
                res.add(getName(idNode));
        } while(!(idNode.getElementType() == FuncIncaTypes.PARENS_CLOSE)); // loop ends when it finds closing parenthesis
        return res;
    }

    public static PsiElement setName(FuncIncaMultipleLet element, List<String> newName){
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID);
        if(idNode != null){
            FuncIncaMultipleLet multipleLet = FuncIncaElementFactory.createMultipleLet(element.getProject(), newName);
            do{
                if(idNode != null && idNode.getElementType() == FuncIncaTypes.ID){
                    // TODO replacing the relevant ID Children from the PSI Element element with the new names from multipleLet
                }
                idNode = idNode.getTreeNext();
            } while(idNode.getElementType() != FuncIncaTypes.PARENS_CLOSE);
        }
        return element;
    }

    public static List<PsiElement> getNameIdentifier(FuncIncaMultipleLet element){
        List<PsiElement> res = new ArrayList<>();
        ASTNode idNode = element.getNode().findChildByType(FuncIncaTypes.ID); // first id of multiple
        res.add(getNameIdentifier(idNode));
        do{
           idNode = idNode.getTreeNext();  // checks type of every child of the multiple let construct
           if(idNode.getElementType() == FuncIncaTypes.ID)
               res.add(getNameIdentifier(idNode));
        } while(!(idNode.getElementType() == FuncIncaTypes.PARENS_CLOSE)); // loop ends when it finds closing parenthesis
        return res;
    }






}
