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

    public LispObject eval(LispNamespace basicNamespace) throws LispException {
        LispObject result = Sexp.getInstance();
        LispNamespace namespace = basicNamespace.prepend(arguments);
        Iterator<LispObject> iterator = body.iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result.eval(basicNamespace);
    }
    
}
