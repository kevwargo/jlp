package kevwargo.jlp.objects.builtins.javareflect;

import java.lang.reflect.Field;
import java.util.HashMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class AccessField extends LispFunction {

    private boolean write;


    private static String buildName(boolean write) {
        return write ? "%set" : "%get";
    }

    private static FormalArguments buildArgs(boolean write) {
        FormalArguments args = new FormalArguments().pos("object").pos("attr");
        if (write) {
            args.pos("value");
        }
        return args;
    }


    public AccessField(boolean write) {
        super(LispType.FUNCTION, buildName(write), buildArgs(write));
        this.write = write;
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject object = arguments.get("object");
        LispObject attr = arguments.get("attr");
        if (attr.isInstance(LispType.INT)) {
            return handleIndex(object, (int)((LispInt)attr.cast(LispType.INT)).getValue(), arguments);
        }
        if (attr.isInstance(LispType.STRING)) {
            return handleField(object, ((LispString)attr.cast(LispType.STRING)).getValue(), arguments);
        }
        throw new LispException("attr argument must be an int or a string");
    }

    private LispObject handleField(LispObject object, String fieldName, HashMap<String, LispObject> arguments) throws LispException {
        Object obj;
        Class<?> cls;

        if (object.isInstance(LispType.JAVA_OBJECT)) {
            obj = ((LispJavaObject)object.cast(LispType.JAVA_OBJECT)).getObject();
            cls = obj.getClass();
        } else if (object.isInstance(LispType.STRING)) {
            try {
                cls = DefaultClassLoader.load(((LispString)object.cast(LispType.STRING)).getValue());
            } catch (ClassNotFoundException e) {
                throw new LispException(e);
            }
            obj = null;
        } else {
            throw new LispException("'object' argument to %s must be a Java-object or a string", this.name);
        }

        try {
            Field field = cls.getField(fieldName);
            if (write) {
                LispObject valueObj = arguments.get("value");
                Object value = ((LispJavaObject)valueObj.cast(LispType.JAVA_OBJECT)).getObject();
                field.set(obj, value);
                return valueObj;
            } else {
                return new LispJavaObject(field.get(obj));
            }
        } catch (Exception e) {
            throw new LispException(e);
        }
    }

    private LispObject handleIndex(LispObject object, int index, HashMap<String, LispObject> arguments) throws LispException {
        Object obj = ((LispJavaObject)object.cast(LispType.JAVA_OBJECT)).getObject();
        if (!obj.getClass().isArray()) {
            throw new LispException("java-object '%s' is not an array", obj);
        }
        Object array[] = (Object[])obj;
        Object value;
        if (write) {
            LispObject valueObj = arguments.get("value");
            value = ((LispJavaObject)valueObj.cast(LispType.JAVA_OBJECT)).getObject();
            array[index] = value;
        } else {
            value = array[index];
        }
        return new LispJavaObject(value);
    }

}
