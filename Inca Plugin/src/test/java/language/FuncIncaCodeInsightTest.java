package language;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.testFramework.TestDataFile;
import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.IOException;

public class FuncIncaCodeInsightTest extends LightJavaCodeInsightFixtureTestCase {

    private String srcPath;
    private String expectPath;

    protected FuncIncaCodeInsightTest(String srcName, String expectName) {
        super();
        srcPath = getDirPath() + '/' + srcName;
        expectPath = getDirPath() + '/' + expectName;
    }

    protected FuncIncaCodeInsightTest(String name) {
        this(name, name);
    }

    /*
    * location of test data*/
    protected static String getDirPath() {
        return "src/test/testData";
    }

    @Override
    protected String getTestDataPath() {
        return srcPath;
    }

    protected String getTestDataPath(String... names){
        String str = getTestDataPath();
        for(String name : names){
            str = str + "/" + name;
        }
        return str;
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    protected String loadFile(@NonNls @TestDataFile String name) throws IOException {
        return doLoadFile(srcPath, name);
    }

    private static String doLoadFile(String myFullDataPath, String name) throws IOException {
        String text = FileUtil.loadFile(new File(myFullDataPath, name), CharsetToolkit.UTF8).trim();
        text = StringUtil.convertLineSeparators(text);
        return text;
    }

    protected void setUpProjectSdk() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                Sdk sdk = getProjectDescriptor().getSdk();
                ProjectJdkTable.getInstance().addJdk(sdk);
                ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
            }
        });
    }
    
}
