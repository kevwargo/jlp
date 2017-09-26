package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class Lambda_M extends Defun_M {

    public Lambda_M() {
        super("lambda", (new FormalArguments()).addPositional("arglist").setRest("body"));
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        Sexp arglist = (Sexp)namespace.resolve("arglist").assertType("sexp");
        Sexp body = (Sexp)namespace.resolve("body").assertType("sexp");
        LispFunction function = new LispFunction("<lambda>", extractArgs(arglist), body, basicNamespace);
        return function;
    }
    
}
