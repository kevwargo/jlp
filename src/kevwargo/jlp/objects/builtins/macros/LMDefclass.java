package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Iterator;
import java.util.Map;

public class LMDefclass extends LispFunction {

    public LMDefclass() {
        super(LispType.MACRO, "defclass", new FormalArguments("name", "bases").rest("body"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        String name = ((LispSymbol) arguments.get("name").cast(LispType.SYMBOL)).getName();

        LispList basesList = (LispList) arguments.get("bases").cast(LispType.LIST);
        LispType bases[];
        if (basesList.size() > 0) {
            bases = new LispType[basesList.size()];
        } else {
            bases = new LispType[] {LispType.OBJECT};
        }
        Iterator<LispObject> it = basesList.iterator();
        for (int i = 0; it.hasNext(); i++) {
            bases[i] = (LispType) it.next().eval(runtime).cast(LispType.TYPE);
        }

        LispNamespace.Layer overlay = new LispNamespace.Layer(true);
        LispRuntime classRuntime = runtime.with(overlay);
        for (LispObject form : (LispList) arguments.get("body")) {
            form.eval(classRuntime);
        }
        LispClass cls = new LispClass(name, bases, overlay);
        runtime.getNS().bind(name, cls);
        return cls;
    }

    private static class LispClass extends LispType {

        LispClass(String name, LispType bases[], Map<String, LispObject> dict) {
            super(name, bases);
            for (Map.Entry<String, LispObject> e : dict.entrySet()) {
                try {
                    setAttr(e.getKey(), e.getValue());
                } catch (LispException exc) {
                }
            }
        }

        public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
                throws LispException {
            LispObject instance = new LispObject(this);
            LispObject constructor = getDict().get("@init@");
            if (constructor != null) {
                arguments.setFirst(instance);
                constructor.call(runtime, arguments);
            }
            defineCastsRecursively(runtime, instance);
            return instance;
        }

        private void defineCastsRecursively(LispRuntime runtime, LispObject instance)
                throws LispException {
            for (LispType type : getBases()) {
                if (type instanceof LispClass) {
                    ((LispClass) type).defineCastsRecursively(runtime, instance);
                } else if (!instance.isCastDefined(type)) {
                    instance.defineCast(type, type.makeInstance(runtime, new ArgumentsIterator()));
                }
            }
        }
    }
}
