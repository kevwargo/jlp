package kevwargo.jlp.objects.builtins.javareflect;

import java.util.HashMap;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LoadClass extends LispFunction {

    public LoadClass() {
        super(LispType.FUNCTION, "%class", new FormalArguments().pos("name"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        String className = ((LispString)arguments.get("name").cast(LispType.STRING)).getValue();
        try {
            return new LispJavaObject(Class.forName(className));
        } catch (ClassNotFoundException e) {
            return LispBool.NIL;
        }
    }

}
