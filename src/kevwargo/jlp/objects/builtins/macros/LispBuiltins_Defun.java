package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinMacro;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.Sexp;
import kevwargo.jlp.utils.FormalArguments;

public class LispBuiltins_Defun extends LispBuiltinMacro {

    public LispBuiltins_Defun() {
        this("defun");
    }

    public LispBuiltins_Defun(String name) {
        super(name, (new FormalArguments()).addPositional("name").addPositional("arglist").setRest("body"));
    }

    protected String extractName() throws LispException {
        LispObject nameObject = arguments.get("name");
        if (!(nameObject instanceof LispSymbol)) {
            throw new LispException("Wrong argument type: name must be a symbol");
        }
        return ((LispSymbol)nameObject).getName();
    }

    protected FormalArguments extractArgs() throws LispException {
        LispObject arglist = arguments.get("arglist");
        if (!(arglist instanceof Sexp)) {
            throw new LispException("Wrong argument type: arglist must be a sexp");
        }
        FormalArguments args = new FormalArguments();
        if (arglist instanceof Sexp) {
            Sexp argsSexp = (Sexp)arglist;
            Iterator<LispObject> iterator = argsSexp.iterator();
            while (iterator.hasNext()) {
                LispObject object = iterator.next();
                if (!(object instanceof LispSymbol)) {
                    throw new LispException(String.format("Formal argument must be a symbol (got %s)", object.toString()));
                } else {
                    LispSymbol symbol = (LispSymbol)object;
                    if (symbol.getName().equals("&rest")) {
                        if (!iterator.hasNext()) {
                            throw new LispException("&rest keyword must be followed by a symbol");
                        }
                        LispObject restObject = iterator.next();
                        if (!(restObject instanceof LispSymbol)) {
                            throw new LispException(String.format("Formal argument must be a symbol (got %s)", restObject.toString()));
                        }
                        args.setRest(((LispSymbol)restObject).getName());
                        break;
                    }
                    args.addPositional(symbol.getName());
                }
            }
        }
        return args;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        String name = extractName();
        namespace.bind(name, new LispFunction(name, extractArgs(), (Sexp)arguments.get("body")));
        return new LispSymbol(name);
    }

}
