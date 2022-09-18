package language;

import com.intellij.testFramework.fixtures.LightJavaCodeInsightFixtureTestCase;

public class FuncIncaCodeInsightTest extends LightJavaCodeInsightFixtureTestCase {
    private String srcPath;
    private String expectPath;

    protected FuncIncaCodeInsightTest(String srcName, String expectName) {
        super();
        srcPath = getDirPath() + '/' + srcName;
        expectPath = getDirPath() + '/' + expectName;
    }

    protected String getTestDataPath() {
        return srcPath;
    }

    protected static String getDirPath(){
        return "tests/testData";
    }
}
