package kevwargo.jlp.objects.builtins.javareflect;

import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Constructor;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class CreateInstance extends Caller {

    public CreateInstance() {
        super(LispType.FUNCTION, "%new", new FormalArguments("args").pos("class"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        Class<?> clazz = (Class<?>)((LispJavaObject)arguments.get("class").cast(LispType.JAVA_OBJECT)).getObject();

        Object args[] = extractObjectArgs(arguments.get("args"));
        Class<?> classes[] = getClasses(args);

        try {
            Constructor constructor = clazz.getConstructor(classes);
            return new LispJavaObject(constructor.newInstance(args));
        } catch (Exception e) {
            throw new LispException(e);
        }
    }

}
