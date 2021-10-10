package kevwargo.jlp.objects.types;

import java.util.Map;

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
        dict.put("@str@", new StrMethod());
        dict.put("@repr@", new ReprMethod());
    }


    private static class StrMethod extends LispFunction {

        StrMethod() {
            super("@str@", new FormalArguments().pos("self"));
            TypeInitializer.instance().deferTypeSet(this, "builtin-function");
        }

        protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            return new LispString(arguments.get("self").toString());
        }

    }

    private static class ReprMethod extends LispFunction {

        ReprMethod() {
            super("@repr@", new FormalArguments().pos("self"));
            TypeInitializer.instance().deferTypeSet(this, "builtin-function");
        }

        protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            return new LispString(arguments.get("self").repr());
        }

    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        return new LispObject();
    }

}
