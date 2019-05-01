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
            LispObject object = iterator.next().cast(LispType.SYMBOL);
            LispSymbol symbol = (LispSymbol)object;
            if (symbol.getName().equals("&rest")) {
                if (!iterator.hasNext()) {
                    throw new LispException("&rest keyword must be followed by a symbol");
                }
                LispObject restObject = iterator.next().cast(LispType.SYMBOL);
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
        String name = ((LispSymbol)arguments.get("name").cast(LispType.SYMBOL)).getName();
        LispList arglist = (LispList)arguments.get("arglist").cast(LispType.LIST);
        LispList body = (LispList)arguments.get("body").cast(LispType.LIST);
        LispFunction function = createFunction(name, buildArgs(arglist), body, namespace);
        namespace.bind(name, function);
        return function;
    }


    protected static class Function extends LispFunction {

        LispList body;
        LispNamespace defNamespace;


        Function(LispType type, String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
            super(type, name, formalArguments);
            this.body = body;
            defNamespace = namespace;
        }

        protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
            Iterator<LispObject> bodyIterator = body.iterator();
            LispObject result = LispBool.NIL;
            HashMap<String, LispObject> map = new HashMap<String, LispObject>();
            map.put("return", new ReturnFunction());
            map.put("$", this);
            LispNamespace bodyNamespace = defNamespace.prepend(arguments).prepend(map);
            try {
                while (bodyIterator.hasNext()) {
                    result = bodyIterator.next().eval(bodyNamespace);
                }
            } catch (ReturnException re) {
                return re.object;
            }
            return result;
        }

    }

    private static class ReturnException extends LispException {

        public LispObject object;

        public ReturnException(LispObject object) {
            super("'return' is used outside of a function");
            this.object = object;
        }

    }

    private static class ReturnFunction extends LispFunction {

        public ReturnFunction() {
            super(LispType.FUNCTION, "return", new FormalArguments().pos("obj"));
        }

        protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
            throw new ReturnException(arguments.get("obj"));
        }

    }

}
