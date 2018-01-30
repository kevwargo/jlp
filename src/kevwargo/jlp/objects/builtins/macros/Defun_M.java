package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Defun_M extends LispFunction {

    public Defun_M() {
        this("defun");
    }

    public Defun_M(String name) {
        this(name, (new FormalArguments()).pos("name").pos("arglist").rest("body"));
    }

    public Defun_M(String name, FormalArguments formalArguments) {
        super(LispType.MACRO, name, formalArguments);
    }

    protected FormalArguments buildArgs(LispList arglist) throws LispException {
        FormalArguments args = new FormalArguments();
        Iterator<LispObject> iterator = arglist.iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().assertType(LispType.SYMBOL);
            LispSymbol symbol = (LispSymbol)object;
            if (symbol.getName().equals("&rest")) {
                if (!iterator.hasNext()) {
                    throw new LispException("&rest keyword must be followed by a symbol");
                }
                LispObject restObject = iterator.next().assertType(LispType.SYMBOL);
                args.rest(((LispSymbol)restObject).getName());
                break;
            }
            args.pos(symbol.getName());
        }
        return args;
    }

    protected LispFunction createFunction(String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
        return new Function(LispType.LISP_FUNCTION, name, formalArguments, body, namespace);
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        String name = ((LispSymbol)arguments.get("name").assertType(LispType.SYMBOL)).getName();
        LispList arglist = (LispList)arguments.get("arglist").assertType(LispType.LIST);
        LispList body = (LispList)arguments.get("body").assertType(LispType.LIST);
        LispFunction function = createFunction(name, buildArgs(arglist), body, namespace);
        namespace.bind(name, function);
        return function;
    }


    protected static class Function extends LispFunction {

        LispList body;
        LispNamespace namespace;


        Function(LispType type, String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
            super(type, name, formalArguments);
            this.body = body;
            this.namespace = namespace;
        }

        protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
            Iterator<LispObject> bodyIterator = body.iterator();
            LispObject result = LispBool.FALSE;
            LispNamespace bodyNamespace = this.namespace.prepend(arguments);
            while (bodyIterator.hasNext()) {
                result = bodyIterator.next().eval(bodyNamespace);
            }
            return result;
        }

    }

}
