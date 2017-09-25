package kevwargo.jlp.objects;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

public class LispFunction extends LispBuiltinFunction {

    private Sexp body;

    public LispFunction(String name, FormalArguments formalArguments, Sexp body) {
        super(name, formalArguments);
        this.body = body;
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        HashMap<String, LispObject> returnMap = new HashMap();
        returnMap.put("return", new ReturnFunction());
        LispNamespace namespace = parseArgs(basicNamespace, arguments).prepend(returnMap);
        LispObject result = Sexp.getInstance();
        Iterator<LispObject> iterator = body.iterator();
        while (iterator.hasNext()) {
            try {
                result = iterator.next().eval(namespace);
            } catch (ReturnException re) {
                return re.getValue();
            }
        }
        return result;
    }

    public String toString() {
        return String.format("function `%s'", name);
    }


    private class ReturnException extends LispException {

        private LispObject value;

        public ReturnException(LispObject value) {
            super("return");
            this.value = value;
        }

        public LispObject getValue() {
            return this.value;
        }
        
    }

    private class ReturnFunction extends LispBuiltinFunction {

        public ReturnFunction() {
            super("name", new FormalArguments().addPositional("value"));
        }

        public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
            throw new ReturnException(parseArgs(basicNamespace, arguments).resolve("value"));
        }
        
    }
    
}
