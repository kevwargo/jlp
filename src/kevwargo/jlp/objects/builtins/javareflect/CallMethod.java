package kevwargo.jlp.objects.builtins.javareflect;

import java.util.HashMap;
import java.util.Iterator;
import java.lang.reflect.Method;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class CallMethod extends Caller {

    public CallMethod() {
        super(LispType.FUNCTION, "%call", new FormalArguments("args").pos("object").pos("method"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        Object obj;
        Class<?> clazz;
        LispObject object = arguments.get("object");

        if (object.isInstance(LispType.JAVA_OBJECT)) {
            obj = ((LispJavaObject)object.cast(LispType.JAVA_OBJECT)).getObject();
            clazz = obj.getClass();
        } else if (object.isInstance(LispType.STRING)) {
            try {
                clazz = Class.forName(((LispString)object.cast(LispType.STRING)).getValue());
            } catch (ClassNotFoundException e) {
                throw new LispException(e);
            }
            obj = null;
        } else {
            throw new LispException("'object' argument to %%call must be a Java-object or a string (%s %s)", object, object.getType().getName());
        }

        String methodName;
        LispObject methodObj = arguments.get("method");
        if (methodObj.isInstance(LispType.STRING)) {
            methodName = ((LispString)methodObj.cast(LispType.STRING)).getValue();
        } else if (methodObj.isInstance(LispType.SYMBOL)) {
            methodName = ((LispSymbol)methodObj.cast(LispType.SYMBOL)).getName();
        } else {
            throw new LispException("'method' argument to %%call must be a symbol or a string");
        }

        Object args[] = extractObjectArgs(arguments.get("args"));
        Class<?> classes[] = getClasses(args);

        try {
            Method method = clazz.getMethod(methodName, classes);
            Object result = method.invoke(obj, args);
            if (result == null) {
                return LispBool.NIL;
            }
            return new LispJavaObject(result);
        } catch (Exception e) {
            throw new LispException(e);
        }
    }

}
