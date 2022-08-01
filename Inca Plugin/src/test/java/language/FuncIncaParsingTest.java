package language;

import com.intellij.testFramework.ParsingTestCase;


public class FuncIncaParsingTest extends ParsingTestCase {

    protected FuncIncaParsingTest() {
        super("", "finca", new FuncIncaParserDefinition());
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
