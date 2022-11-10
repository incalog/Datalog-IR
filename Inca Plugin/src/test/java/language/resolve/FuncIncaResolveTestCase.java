package language.resolve;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import language.FuncIncaCodeInsightTest;
import org.junit.jupiter.api.BeforeEach;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class FuncIncaResolveTestCase extends FuncIncaCodeInsightTest {
    protected PsiReference refElement;
    protected PsiElement resolvedElement;

    public FuncIncaResolveTestCase(String srcName) {
        super(srcName, srcName);
    }

    public FuncIncaResolveTestCase() {
        this("resolve");
    }

    @Override
    @BeforeEach
    protected void setUp() throws Exception {
        super.setUp();
        for (File file : getTestDataFiles(new File(getTestDataPath()))) {
            if (file.isDirectory()) continue;
            String text = FileUtil.loadFile(file, CharsetToolkit.UTF8);
            text = StringUtil.convertLineSeparators(text);
            int referencedOffset = text.indexOf("<ref>");
            int resolvedOffset = text.indexOf("<resolved>");
            if(referencedOffset < resolvedOffset){
                text = text.replace("<resolved>", "");
                text = text.replace("<ref>", "");
                if(referencedOffset != -1)
                    resolvedOffset -= 5; // removal of <ref> shifts the offset of <resolved> by 5
            }else{
                text = text.replace("<ref>", "");
                text = text.replace("<resolved>", "");
                if(resolvedOffset != -1)
                    referencedOffset -= 10; // removal of <resolved> shifts offset of <ref> by 10
            }
            String fileName = file.getName();
            VirtualFile vFile = myFixture.getTempDirFixture().createFile(fileName, text);
            PsiFile psiFile = myFixture.configureFromTempProjectFile(fileName);
            if (referencedOffset != -1) {
                refElement = psiFile.findReferenceAt(referencedOffset);
                if (refElement == null) fail("Reference was null in " + file.getName());
            }
            if (resolvedOffset != -1) {
                resolvedElement = psiFile.findElementAt(resolvedOffset); // returns FuncIncaId
                resolvedElement = resolvedElement.getParent(); // parent of the Id Node is a FuncIncaVarId Node
                if (resolvedElement == null) { fail("Resolved element returned null in " + file.getName()); }
            }
        }
    }

    @Override
    protected String getTestDataPath(){
        String path = super.getTestDataPath() + "/" + getTestName(false);
        return path;
    }

    protected Collection<File> getTestDataFiles(File dir){
        List<File> testData = new ArrayList<>();
        for (File entry : dir.listFiles()) {
            if (entry.isDirectory()) {
                getTestDataFiles(entry);
            } else {
                testData.add(entry);
            }
        }
        return testData;
    }

    protected void doTest() {
        doTest(true);
    }

    protected void doTest(boolean succeed) {
        if (succeed && refElement == null) { fail("Could not find reference at caret."); }
        if (succeed && resolvedElement == null) { fail("Could not find resolved element."); }
        if (succeed) {
            PsiElement resolvedActual = refElement.resolve();
            assertEquals(
                    "Could not resolve expected reference.\n" +
                            "Expected: " + resolvedElement + " (" + resolvedElement.getText() + ")\n" +
                            "Actual: " + resolvedActual + " (" + resolvedActual.getText() + ")",
                    resolvedElement,
                    resolvedActual
            );
        } else {
            assertFalse("Resolved unexpected reference.", resolvedElement.equals(refElement.resolve()));
        }
    }

}
