package kevwargo.jlp.objects.builtins.macros;

import java.util.Map;
import java.util.Iterator;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Lambda_M extends Defun_M {

    public Lambda_M() {
        super("lambda", (new FormalArguments()).pos("arglist").rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispList arglist = (LispList)arguments.get("arglist").cast(LispType.LIST);
        LispList body = (LispList)arguments.get("body").cast(LispType.LIST);
        LispFunction function = createFunction("<lambda>", buildArgs(arglist), body, namespace);
        return function;
    }    
}
