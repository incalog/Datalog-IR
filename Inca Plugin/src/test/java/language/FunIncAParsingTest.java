package language;

import com.intellij.testFramework.ParsingTestCase;


public class FunIncAParsingTest extends ParsingTestCase {

    protected FunIncAParsingTest() {
        super("", "finca", new FunIncAParserDefinition());
    }

    public void testParsingTestData() {
        doTest(true);
    }

    @Override
    protected String getTestDataPath() {
        return "src/test/testData";
    }

    @Override
    protected boolean skipSpaces() {
        return false;
    }

    @Override
    protected boolean includeRanges() {
        return true;
    }
}
