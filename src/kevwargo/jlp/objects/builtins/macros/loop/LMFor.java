package kevwargo.jlp.objects.builtins.macros.loop;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispIterable;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class LMFor extends LoopBase {

    public static final String NAME = "for";
    public static final String ARG_COND = "cond";
    public static final String ARG_BODY = "body";

    public LMFor() {
        super(NAME, new FormalArguments(ARG_COND).rest(ARG_BODY));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispList cond = (LispList) arguments.get(ARG_COND).cast(LispType.LIST);
        if (cond.size() < 1 || cond.size() > 3) {
            throw new LispException("'for' loop condition must contain 1, 2 or 3 elements");
        }

        LispList body = (LispList) arguments.get(ARG_BODY).cast(LispType.LIST);
        LispList collector = new LispList();

        switch (cond.size()) {
            case 1:
                executeWhile(cond.iterator().next(), body, runtime, collector);
                break;
            case 2:
                executeIterator(cond, body, runtime, collector);
                break;
            default:
                executeSimple(cond, body, runtime, collector);
        }

        return collector;
    }

    private void executeWhile(
            LispObject cond, LispList body, LispRuntime runtime, LispList collector)
            throws LispException {
        while (cond.eval(runtime) != LispBool.NIL) {
            if (executeBody(runtime, body, collector)) {
                return;
            }
        }
    }

    private void executeSimple(
            LispList forCond, LispList body, LispRuntime runtime, LispList collector)
            throws LispException {
        LispObject init = forCond.get(0);
        LispObject cond = forCond.get(1);
        LispObject incr = forCond.get(2);
        init.eval(runtime);
        while (cond.eval(runtime) != LispBool.NIL) {
            if (executeBody(runtime, body, collector)) {
                return;
            }
            incr.eval(runtime);
        }
    }

    private void executeIterator(
            LispList cond, LispList body, LispRuntime runtime, LispList collector)
            throws LispException {
        String var = ((LispSymbol) cond.get(0).cast(LispType.SYMBOL)).getName();
        LispObject it = cond.get(1).eval(runtime);

        if (it.isInstance(LispType.JAVA_OBJECT)) {
            executeJavaIterator(
                    var, (LispJavaObject) it.cast(LispType.JAVA_OBJECT), body, runtime, collector);
        } else if (it instanceof LispIterable) {
            executeLispIterator(var, ((LispIterable) it).iterator(), body, runtime, collector);
        } else {
            throw new LispException("object '%s' is not iterable", it.getType().getName());
        }
    }

    private void executeLispIterator(
            String var,
            Iterator<LispObject> it,
            LispList body,
            LispRuntime runtime,
            LispList collector)
            throws LispException {
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        while (it.hasNext()) {
            map.put(var, it.next());
            if (executeBody(runtime.with(map), body, collector)) {
                return;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void executeJavaIterator(
            String var,
            LispJavaObject javaObject,
            LispList body,
            LispRuntime runtime,
            LispList collector)
            throws LispException {
        Object object = javaObject.getObject();

        if (object.getClass().isArray()) {
            executeJavaArray(var, (Object[]) object, body, runtime, collector);
            return;
        }

        Iterator<Object> it;
        if (object instanceof Iterator) {
            it = (Iterator) object;
        } else if (object instanceof Iterable) {
            it = ((Iterable) object).iterator();
        } else {
            throw new LispException(
                    "Java object %s of type %s is not iterable", object, object.getClass());
        }

        Map<String, LispObject> map = new HashMap<String, LispObject>();
        while (it.hasNext()) {
            map.put(var, new LispJavaObject(it.next()));
            if (executeBody(runtime.with(map), body, collector)) {
                return;
            }
        }
    }

    private void executeJavaArray(
            String var, Object array[], LispList body, LispRuntime runtime, LispList collector)
            throws LispException {
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        for (Object object : array) {
            map.put(var, new LispJavaObject(object));
            if (executeBody(runtime.with(map), body, collector)) {
                return;
            }
        }
    }
}
