package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBaseObject;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispMethod;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

import java.util.Iterator;
import java.util.Map;

public class LMDefclass extends LispFunction {

    public LMDefclass() {
        super(LispType.MACRO, "defclass", new CallArgs("name", "bases").rest("body"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        String name = ((LispSymbol) args.get("name").cast(LispType.SYMBOL)).getName();

        LispList basesList = (LispList) args.get("bases").cast(LispType.LIST);
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
        for (LispObject form : (LispList) args.get("body")) {
            form.eval(classRuntime);
        }
        LispClass cls = new LispClass(name, bases, overlay);
        runtime.getNS().bind(name, cls);
        return cls;
    }

    private static class LispClass extends LispType {

        private static final String ARG_ARGLIST = "arglist";

        LispClass(String name, LispType bases[], Map<String, LispObject> dict) {
            super(name, bases, new CallArgs().rest(ARG_ARGLIST));
            for (Map.Entry<String, LispObject> e : dict.entrySet()) {
                try {
                    setAttr(e.getKey(), e.getValue());
                } catch (LispException exc) {
                }
            }
        }

        public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
            LispBaseObject instance = new LispBaseObject(this);
            LispObject constructor = instance.getAttr("@init@");
            if (constructor instanceof LispMethod) {
                LispList arglist = (LispList) args.get(ARG_ARGLIST);
                LispMethod method = (LispMethod) constructor;
                method.call(runtime, method.getCallArgs().apply(arglist));
            }
            defineCastsRecursively(runtime, instance);
            return instance;
        }

        private void defineCastsRecursively(LispRuntime runtime, LispBaseObject instance)
                throws LispException {
            for (LispType type : getBases()) {
                if (type instanceof LispClass) {
                    ((LispClass) type).defineCastsRecursively(runtime, instance);
                } else if (!instance.isCastDefined(type)) {
                    instance.defineCast(type, type.call(runtime, new LispNamespace.Layer()));
                }
            }
        }
    }
}
