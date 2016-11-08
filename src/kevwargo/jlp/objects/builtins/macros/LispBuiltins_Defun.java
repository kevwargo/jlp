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

public class LispBuiltins_Defun extends LispBuiltinMacro {

    public LispBuiltins_Defun() {
        this("defun");
    }

    public LispBuiltins_Defun(String name) {
        super(name, new String[] {"name", "arglist"}, true);
    }

    protected String extractName() throws LispException {
        LispObject nameObject = arguments.get("name");
        if (!(nameObject instanceof LispSymbol)) {
            throw new LispException("Wrong argument type: name must be a symbol");
        }
        return ((LispSymbol)nameObject).getName();
    }

    protected String[] extractArgs() throws LispException {
        LispObject arglist = arguments.get("arglist");
        if (!(arglist instanceof Sexp) && !(arglist instanceof LispNil)) {
            throw new LispException("Wrong argument type: arglist must be a sexp or nil");
        }
        String args[];
        if (arglist instanceof Sexp) {
            Sexp argsSexp = (Sexp)arglist;
            args = new String[argsSexp.size()];
            ListIterator<LispObject> iterator = argsSexp.listIterator();
            int i = 0;
            while (iterator.hasNext()) {
                LispObject object = iterator.next();
                if (!(object instanceof LispSymbol)) {
                    throw new LispException(String.format("Formal argument must be a symbol (got %s)", object.toString()));
                } else {
                    args[i++] = ((LispSymbol)object).getName();
                }
            }
        } else {
            args = new String[0];
        }
        return args;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        String name = extractName();
        namespace.bind(name, new LispFunction(name, extractArgs(), false, rest));
        return new LispSymbol(name);
    }

}
