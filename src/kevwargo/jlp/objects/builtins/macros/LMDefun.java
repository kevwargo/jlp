package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class LMDefun extends LispFunction {

    public static String NAME = "defun";
    public static String ARG_NAME = "name";
    public static String ARG_ARGLIST = "arglist";
    public static String ARG_BODY = "body";

    public LMDefun() {
        this(NAME);
    }

    public LMDefun(String name) {
        this(name, new FormalArguments(ARG_NAME, ARG_ARGLIST).rest(ARG_BODY));
    }

    public LMDefun(String name, FormalArguments formalArguments) {
        super(LispType.MACRO, name, formalArguments);
    }

    protected FormalArguments buildArgs(LispList arglist) throws LispException {
        FormalArguments args = new FormalArguments();
        Iterator<LispObject> iterator = arglist.iterator();
        while (iterator.hasNext()) {
            LispObject object = iterator.next().cast(LispType.SYMBOL);
            LispSymbol symbol = (LispSymbol) object;
            if (symbol.getName().equals("&rest")) {
                if (!iterator.hasNext()) {
                    throw new LispException("&rest keyword must be followed by a symbol");
                }
                LispObject restObject = iterator.next().cast(LispType.SYMBOL);
                args.rest(((LispSymbol) restObject).getName());
                break;
            }
            args.pos(symbol.getName());
        }
        return args;
    }

    protected LispFunction createFunction(
            String name, FormalArguments formalArguments, LispList body, LispNamespace namespace) {
        return new Function(LispType.LISP_FUNCTION, name, formalArguments, body, namespace);
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        String name = ((LispSymbol) arguments.get(ARG_NAME).cast(LispType.SYMBOL)).getName();
        LispList arglist = (LispList) arguments.get(ARG_ARGLIST).cast(LispType.LIST);
        LispList body = (LispList) arguments.get(ARG_BODY).cast(LispType.LIST);

        LispNamespace namespace = runtime.getNS();
        LispFunction function = createFunction(name, buildArgs(arglist), body, namespace);
        namespace.bind(name, function);

        return function;
    }

    protected static class Function extends LispFunction {

        LispList body;
        LispNamespace defNamespace;

        public Function(
                LispType type,
                String name,
                FormalArguments formalArguments,
                LispList body,
                LispNamespace namespace) {
            super(type, name, formalArguments);
            this.body = body;
            defNamespace = namespace;
        }

        protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
                throws LispException {
            Map<String, LispObject> map = new HashMap<String, LispObject>();
            map.put("return", new ReturnFunction());
            map.put("$", this);
            LispNamespace bodyNamespace = defNamespace.with(arguments).with(map);

            Iterator<LispObject> bodyIterator = body.iterator();
            LispObject result = LispNil.NIL;
            try {
                while (bodyIterator.hasNext()) {
                    result = bodyIterator.next().eval(runtime.with(bodyNamespace));
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
            super(LispType.FUNCTION, "return", new FormalArguments("obj"));
        }

        protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
                throws LispException {
            throw new ReturnException(arguments.get("obj"));
        }
    }
}
