package kevwargo.jlp.objects.builtins.macros.loop;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LMFor extends LoopBase {

    public LMFor() {
        super("for", new FormalArguments("cond").rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispList cond = (LispList)arguments.get("cond").cast(LispType.LIST);
        if (cond.size() < 1 || cond.size() > 3) {
            throw new LispException("'for' loop condition must contain 1-3 elements");
        }

        LispList body = (LispList)arguments.get("body").cast(LispType.LIST);
        switch (cond.size()) {
        case 1:
            return executeWhile(cond.iterator().next(), body, namespace);
        case 2:
            return executeIterator(cond, body, namespace);
        default:
            return executeSimple(cond, body, namespace);
        }
    }

    private LispObject executeWhile(LispObject cond, LispList body, LispNamespace namespace) throws LispException {
        while (cond.eval(namespace) != LispBool.NIL) {
            if (executeBody(namespace, body)) {
                break;
            }
        }
        return LispBool.NIL;
    }

    private LispObject executeSimple(LispList forCond, LispList body, LispNamespace namespace) throws LispException {
        LispObject init = forCond.get(0);
        LispObject cond = forCond.get(1);
        LispObject incr = forCond.get(2);
        init.eval(namespace);
        while (cond.eval(namespace) != LispBool.NIL) {
            if (executeBody(namespace, body)) {
                break;
            }
            incr.eval(namespace);
        }
        return LispBool.NIL;
    }

    private LispObject executeIterator(LispList cond, LispList body, LispNamespace namespace) throws LispException {
        String var = ((LispSymbol)cond.get(0).cast(LispType.SYMBOL)).getName();
        LispObject iterator = cond.get(1).eval(namespace);

        if (iterator.isInstance(LispType.LIST)) {
            return executeList(var, (LispList)iterator.cast(LispType.LIST), body, namespace);
        }
        if (iterator.isInstance(LispType.STRING)) {
            return executeString(var, (LispString)iterator.cast(LispType.STRING), body, namespace);
        }
        if (iterator.isInstance(LispType.JAVA_OBJECT)) {
            return executeJavaIterator(var, (LispJavaObject)iterator.cast(LispType.JAVA_OBJECT), body, namespace);
        }
        throw new LispException("object of type '%s' is not iterable", iterator.getType().getName());
    }

    private LispObject executeList(String var, LispList list, LispList body, LispNamespace namespace) throws LispException {
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        for (LispObject val : list) {
            map.put(var, val);
            if (executeBody(namespace.prepend(map), body)) {
                break;
            }
        }
        return LispBool.NIL;
    }

    private LispObject executeString(String var, LispString lispString, LispList body, LispNamespace namespace) throws LispException {
        String string = lispString.getValue();
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        int length = string.length();
        for (int i = 0; i < length; i++) {
            map.put(var, new LispInt(string.codePointAt(i)));
            if (executeBody(namespace.prepend(map), body)) {
                break;
            }
        }
        return LispBool.NIL;
    }

    @SuppressWarnings("unchecked")
    private LispObject executeJavaIterator(String var, LispJavaObject javaObject, LispList body, LispNamespace namespace) throws LispException {
        Object object = javaObject.getObject();

        if (object.getClass().isArray()) {
            return executeJavaArray(var, (Object[])object, body, namespace);
        }

        Iterator<Object> it;
        if (object instanceof Iterator) {
            it = (Iterator)object;
        } else if (object instanceof Iterable) {
            it = ((Iterable)object).iterator();
        } else {
            throw new LispException("Java object %s of type %s is not iterable", object, object.getClass());
        }

        Map<String, LispObject> map = new HashMap<String, LispObject>();
        while (it.hasNext()) {
            map.put(var, new LispJavaObject(it.next()));
            if (executeBody(namespace.prepend(map), body)) {
                break;
            }
        }
        return LispBool.NIL;
    }

    private LispObject executeJavaArray(String var, Object array[], LispList body, LispNamespace namespace) throws LispException {
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        for (Object object : array) {
            map.put(var, new LispJavaObject(object));
            if (executeBody(namespace.prepend(map), body)) {
                break;
            }
        }
        return LispBool.NIL;
    }

}
