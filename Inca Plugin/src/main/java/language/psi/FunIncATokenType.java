package language.psi;

import com.intellij.psi.tree.IElementType;
import language.FunIncALanguage;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class FunIncATokenType extends IElementType {

    public FunIncATokenType(@NotNull @NonNls String debugName) {
        super(debugName, FunIncALanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "FuncIncaTokenType." + super.toString();
    }

}