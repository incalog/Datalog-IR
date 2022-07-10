package language;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

public class FuncIncaColorSettingsPage implements ColorSettingsPage{

    private static final AttributesDescriptor[] DESCRIPTORS = new AttributesDescriptor[] {
            new AttributesDescriptor("Reserved IDs", FuncIncaSyntaxHighlighter.RESERVED_ID),
            new AttributesDescriptor("Reserved symbols", FuncIncaSyntaxHighlighter.RESERVED_OP),
            new AttributesDescriptor("Comma", FuncIncaSyntaxHighlighter.COMMA),
            new AttributesDescriptor("Squared brackets", FuncIncaSyntaxHighlighter.BRACKETS),
            new AttributesDescriptor("Braces", FuncIncaSyntaxHighlighter.BRACES),
            new AttributesDescriptor("Parenthesis", FuncIncaSyntaxHighlighter.PARENTHESES),
            new AttributesDescriptor("Comment", FuncIncaSyntaxHighlighter.COMMENT),
            new AttributesDescriptor("Integer or long", FuncIncaSyntaxHighlighter.INTEGER),
            new AttributesDescriptor("Double", FuncIncaSyntaxHighlighter.DOUBLE),
            new AttributesDescriptor("String", FuncIncaSyntaxHighlighter.STRING),
            new AttributesDescriptor("Operation Symbols", FuncIncaSyntaxHighlighter.OPSYM),
            new AttributesDescriptor("Scalaterm", FuncIncaSyntaxHighlighter.SCALATERM)
    };

    @Override
    public @Nullable Icon getIcon() {
        return FuncIncaIcons.FILE;
    }

    @Override
    public @NotNull SyntaxHighlighter getHighlighter() {
        return new FuncIncaSyntaxHighlighter();
    }

    @Override
    public @NotNull String getDemoText() {
        return  "module demo\n" +
                "import another_module\n" +
                "\n" +
                "data Tree[Int] = Leaf() | lhs(Int, Tree[Int], Tree[Int]) | rhs(Int, Tree[Int], Tree[Int])\n" +
                "\n" +
                "def hello_world(): String = \"Hello World\"\n" +
                "\n" +
                "def lambda: Int = ((x: Int) => x * 3)(7)\n" +
                "\n" +
                "@main def main(s: Set): Set = s match{\n" +
                "\t case Nothing => {}\n" +
                "\t case Any => s\n" +
                "\n" +
                "def inc(x: Int): Int = x + 1\n";
    }

    @Override
    public @Nullable Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return null;
    }

    @Override
    public AttributesDescriptor @NotNull [] getAttributeDescriptors() {
        return DESCRIPTORS;
    }

    @Override
    public ColorDescriptor @NotNull [] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }

    @Override
    public @NotNull String getDisplayName() {
        return "Functional Inca";
    }
}
