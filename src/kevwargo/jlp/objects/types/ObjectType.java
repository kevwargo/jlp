package kevwargo.jlp.objects.types;

import java.util.HashMap;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class ObjectType extends LispType {

    ObjectType() {
        super("object");
    }

    void init() {
        dict.put("@str@", new StrMethod());
        dict.put("@repr@", new ReprMethod());
    }


    private static class StrMethod extends LispFunction {

        StrMethod() {
            super(LispType.FUNCTION, "@str@", new FormalArguments().pos("self"));
        }

        protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
            return new LispString(arguments.get("self").toString());
        }

    }

    private static class ReprMethod extends LispFunction {

        ReprMethod() {
            super(LispType.FUNCTION, "@repr@", new FormalArguments().pos("self"));
        }

        protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
            return new LispString(arguments.get("self").repr());
        }

    }

    protected LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        return new LispObject();
    }

}
