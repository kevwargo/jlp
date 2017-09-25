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

public class Defun_M extends LispBuiltinMacro {

    public Defun_M() {
        this("defun");
    }

    public Defun_M(String name) {
        this(name, (new FormalArguments()).addPositional("name").addPositional("arglist").setRest("body"));
    }

    public Defun_M(String name, FormalArguments formalArguments) {
        super(name, formalArguments);
    }

    protected FormalArguments extractArgs(Sexp arglist) throws LispException {
        FormalArguments args = new FormalArguments();
        Iterator<LispObject> iterator = arglist.iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().assertType("symbol");
            LispSymbol symbol = (LispSymbol)object;
            if (symbol.getName().equals("&rest")) {
                if (!iterator.hasNext()) {
                    throw new LispException("&rest keyword must be followed by a symbol");
                }
                LispObject restObject = iterator.next().assertType("symbol");
                args.setRest(((LispSymbol)restObject).getName());
                break;
            }
            args.addPositional(symbol.getName());
        }
        return args;
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        String name = ((LispSymbol)namespace.resolve("name").assertType("symbol")).getName();
        Sexp arglist = (Sexp)namespace.resolve("arglist").assertType("sexp");
        Sexp body = (Sexp)namespace.resolve("body").assertType("sexp");
        LispFunction function = new LispFunction(name, extractArgs(arglist), body);
        basicNamespace.bind(name, function);
        return function;
    }

}
