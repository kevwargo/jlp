package kevwargo.jlp.objects.builtins.functions;

import java.util.ListIterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.Sexp;


public class LispBuiltins_Append extends LispBuiltinFunction {

    public LispBuiltins_Append() {
        super("append", new String[0], true);
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        Sexp result = new Sexp();
        for (LispObject object : rest) {
            if (!(object instanceof Sexp)) {
                throw new LispException("Only lists can be appended");
            }
            ListIterator<LispObject> iterator = ((Sexp)object).listIterator();
            while (iterator.hasNext()) {
                result.add(iterator.next());
            }
        }
        return result.size() > 0 ? result : LispNil.getInstance();
    }
    
}
