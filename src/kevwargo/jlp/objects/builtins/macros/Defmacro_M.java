package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispMacro;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;


public class Defmacro_M extends Defun_M {

    public Defmacro_M() {
        super("defmacro");
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        String name = ((LispSymbol)namespace.resolve("name").assertType("symbol")).getName();
        Sexp arglist = (Sexp)namespace.resolve("arglist").assertType("sexp");
        Sexp body = (Sexp)namespace.resolve("body").assertType("sexp");
        LispMacro macro = new LispMacro(name, extractArgs(arglist), body, basicNamespace);
        basicNamespace.bind(name, macro);
        return macro;
    }
    
}
