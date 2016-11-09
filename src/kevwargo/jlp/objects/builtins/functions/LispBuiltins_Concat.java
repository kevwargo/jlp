package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;
import kevwargo.jlp.objects.LispBuiltinFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;

public class LispBuiltins_Concat extends LispBuiltinFunction {

    public LispBuiltins_Concat() {
        super("concat", new String[0], true);
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        String result = "";
        for (LispObject object : rest) {
            if (!(object instanceof LispString)) {
                throw new LispException("Wrong argument type: string expected");
            }
            result += ((LispString)object).getValue();
        }
        return new LispString(result);
    }
    
}
