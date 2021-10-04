package kevwargo.jlp.objects.builtins.javareflect;

import java.lang.reflect.Constructor;
import java.util.HashMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class CreateInstance extends LispFunction {

    public CreateInstance() {
        super(LispType.FUNCTION, "%new", new FormalArguments("args").pos("class"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        Class<?> cls;
        LispObject classArg = arguments.get("class");
        if (classArg.isInstance(LispType.JAVA_OBJECT)) {
            cls = (Class<?>)((LispJavaObject)classArg.cast(LispType.JAVA_OBJECT)).getObject();
        } else if (classArg.isInstance(LispType.STRING)) {
            try {
                cls = Class.forName(((LispString)classArg.cast(LispType.STRING)).getValue());
            } catch (ClassNotFoundException e) {
                throw new LispException(e);
            }
        } else{
            throw new LispException("'class' argument must be either a string (class name) or a Java object representing a class (java.lang.Class instance)");
        }

        ReflectedArgs args = new ReflectedArgs(arguments.get("args"));

        try {
            Constructor constructor = cls.getConstructor(args.getClasses());
            return new LispJavaObject(constructor.newInstance(args.getObjects()));
        } catch (Exception e) {
            throw new LispException(e);
        }
    }

}
