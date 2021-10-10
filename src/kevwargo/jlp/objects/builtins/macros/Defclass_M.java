package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Defclass_M extends LispFunction {

    public Defclass_M() {
        super(LispType.MACRO, "defclass", new FormalArguments().pos("name").pos("bases").rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        String name = ((LispSymbol)arguments.get("name").cast(LispType.SYMBOL)).getName();

        LispList basesList = (LispList)arguments.get("bases").cast(LispType.LIST);
        LispType bases[];
        if (basesList.size() > 0) {
            bases = new LispType[basesList.size()];
        } else {
            bases = new LispType[] { LispType.OBJECT };
        }
        Iterator<LispObject> it = basesList.iterator();
        for (int i = 0; it.hasNext(); i++) {
            bases[i] = (LispType)it.next().eval(namespace).cast(LispType.TYPE);
        }
        
        Overlay overlay = new Overlay();
        LispNamespace classNamespace = namespace.prepend(overlay);
        for (LispObject form : (LispList)arguments.get("body")) {
            form.eval(classNamespace);
        }
        Map<String, LispObject> dict = new HashMap<String, LispObject>();
        dict.putAll(overlay);
        LispClass cls = new LispClass(name, bases, dict);
        namespace.bind(name, cls);
        return cls;
    }


    private static class Overlay extends HashMap<String, LispObject> {
        public boolean containsKey(Object key) {
            return true;
        }
    }

    private static class LispClass extends LispType {

        LispClass(String name, LispType bases[], Map<String, LispObject> dict) {
            super(name);
            setType(LispType.TYPE);
            setBaseTypes(bases);
            this.dict = dict;
        }

        @Override
        public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
            LispObject instance = new LispObject();
            instance.setType(this);
            LispObject constructor = dict.get("@init@");
            if (constructor != null) {
                arguments.setFirst(instance);
                constructor.call(namespace, arguments);
            }
            defineCastsRecursively(namespace, instance);
            return instance;
        }

        private void defineCastsRecursively(LispNamespace namespace, LispObject instance) throws LispException {
            for (LispType type : baseTypes) {
                if (type instanceof LispClass) {
                    ((LispClass)type).defineCastsRecursively(namespace, instance);
                } else if (! instance.isCastDefined(type)) {
                    instance.defineCast(type, type.makeInstance(namespace, new ArgumentsIterator()));
                }
            }
        }

    }

}
