package kevwargo.jlp.objects;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

public class LispMacro extends LispBuiltinMacro {

    private Sexp body;
    private LispNamespace closureNamespace;

    public LispMacro(String name, FormalArguments formalArguments, Sexp body, LispNamespace closureNamespace) {
        super(name, formalArguments);
        this.body = body;
        this.closureNamespace = closureNamespace;
    }

    public LispObject call(LispNamespace basicNamespace, Iterator<LispObject> arguments) throws LispException {
        LispNamespace namespace = parseArgs(closureNamespace, arguments);
        LispObject result = Sexp.getInstance();
        Iterator<LispObject> iterator = body.iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result.eval(basicNamespace);
    }

    public String toString() {
        return String.format("Lisp macro `%s'", name);
    }
    
}
