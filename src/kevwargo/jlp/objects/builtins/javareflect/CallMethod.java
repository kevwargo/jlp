package kevwargo.jlp.objects.builtins.javareflect;

import java.lang.reflect.Method;
import java.util.HashMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class CallMethod extends LispFunction {

    public CallMethod() {
        super(LispType.FUNCTION, "%call", new FormalArguments("args").pos("object").pos("method"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        Object object;
        Class<?> cls;
        LispObject lispObject = arguments.get("object");

        if (lispObject.isInstance(LispType.JAVA_OBJECT)) {
            object = ((LispJavaObject)lispObject.cast(LispType.JAVA_OBJECT)).getObject();
            cls = object.getClass();
        } else if (lispObject.isInstance(LispType.STRING)) {
            try {
                cls = DefaultClassLoader.load(((LispString)lispObject.cast(LispType.STRING)).getValue());
            } catch (ClassNotFoundException e) {
                throw new LispException(e);
            }
            object = null;
        } else {
            throw new LispException("'object' argument to %%call must be a Java-object or a string (%s %s)", lispObject, lispObject.getType().getName());
        }

        String methodName = extractMethodName(arguments.get("method"));
        ReflectedArgs args = new ReflectedArgs(arguments.get("args"));

        return invoke(cls, object, methodName, args);
    }

    private String extractMethodName(LispObject arg) throws LispException {
        if (arg.isInstance(LispType.STRING)) {
            return ((LispString)arg.cast(LispType.STRING)).getValue();
        }
        if (arg.isInstance(LispType.SYMBOL)) {
            return ((LispSymbol)arg.cast(LispType.SYMBOL)).getName();
        }

        throw new LispException("'method' argument to %%call must be a symbol or a string");
    }

    private LispObject invoke(Class<?> cls, Object object, String methodName, ReflectedArgs args) throws LispException {
        try {
            Method method = cls.getMethod(methodName, args.getClasses());
            Object result = method.invoke(object, args.getObjects());
            if (result == null) {
                return LispBool.NIL;
            }
            return new LispJavaObject(result);
        } catch (Exception e) {
            throw new LispException(e);
        }
    }

}
