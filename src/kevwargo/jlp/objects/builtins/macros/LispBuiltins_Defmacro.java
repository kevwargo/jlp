package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;


public class LispBuiltins_Defmacro extends LispBuiltins_Defun {

    public LispBuiltins_Defmacro() {
        super("defmacro");
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        String name = extractName();
        namespace.bind(name, new LispMacro(name, extractArgs(), (Sexp)arguments.get("body")));
        return new LispSymbol(name);
    }
    
}
