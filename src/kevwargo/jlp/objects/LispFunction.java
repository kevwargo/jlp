package kevwargo.jlp.objects;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

public class LispFunction extends LispBuiltinFunction {

    private Sexp body;

    public LispFunction(String name, FormalArguments formalArguments, Sexp body) {
        super(name, formalArguments);
        this.body = body;
    }

    public LispObject eval(LispNamespace basicNamespace) throws LispException {
        LispObject result = LispNil.getInstance();
        LispNamespace namespace = basicNamespace.prepend(arguments);
        Iterator<LispObject> iterator = body.iterator();
        while (iterator.hasNext()) {
            result = iterator.next().eval(namespace);
        }
        return result;
    }
    
}
