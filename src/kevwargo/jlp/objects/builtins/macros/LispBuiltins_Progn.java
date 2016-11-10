package kevwargo.jlp.objects.builtins.macros;

import java.util.ListIterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;

public class LispBuiltins_Progn extends LispBuiltinMacro {

    public LispBuiltins_Progn() {
        super("progn", new String[0], true);
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        LispObject result = LispNil.getInstance();
        for (LispObject object : rest) {
            result = object.eval(namespace);
        }
        return result;
    }
    
}
