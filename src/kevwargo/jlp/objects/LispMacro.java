package kevwargo.jlp.objects;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

public class LispMacro extends LispBuiltinMacro {

    private Sexp body;

    public LispMacro(String name, FormalArguments formalArguments, Sexp body) {
        super(name, formalArguments);
        this.body = body;
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(basicNamespace, arguments);
        LispObject result = Sexp.getInstance();
        Iterator<LispObject> iterator = body.iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result.eval(basicNamespace);
    }

    public String toString() {
        return String.format("macro `%s'", name);
    }
    
}
