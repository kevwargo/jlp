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

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        String name = ((LispSymbol)arguments.get("name").cast(LispType.SYMBOL)).getName();
        LispList basesList = (LispList)arguments.get("bases").cast(LispType.LIST);
        LispType bases[] = new LispType[basesList.size()];
        int pos = 0;
        for (LispObject base : basesList) {
            bases[pos++] = (LispType)base.eval(namespace).cast(LispType.TYPE);
        }
        Overlay overlay = new Overlay();
        LispNamespace classNamespace = namespace.prepend(overlay);
        for (LispObject form : (LispList)arguments.get("body")) {
            form.eval(classNamespace);
        }
        HashMap<String, LispObject> dict = new HashMap<String, LispObject>();
        dict.putAll(overlay);
        LispClass klass = new LispClass(name, bases, dict);
        namespace.bind(name, klass);
        return klass;
    }


    private static class Overlay extends HashMap<String, LispObject> {
        public boolean containsKey(Object key) {
            return true;
        }
    }

    private static class LispClass extends LispType {

        LispClass(String name, LispType bases[], HashMap<String, LispObject> dict) {
            super(LispType.TYPE, name, bases);
            this.dict = dict;
        }

        @Override
        public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
            LispObject instance = new LispObject(this);
            defineCastsRecursively(namespace, instance);
            LispObject constructor = instance.getAttr("@init@", false);
            if (constructor != null) {
                constructor.call(namespace, arguments);
            }
            return instance;
        }

        private void defineCastsRecursively(LispNamespace namespace, LispObject instance) throws LispException {
            for (LispType type : baseTypes) {
                if (type instanceof LispClass) {
                    ((LispClass)type).defineCastsRecursively(namespace, instance);
                } else {
                    instance.defineCast(type, type.makeInstance(namespace, new ArgumentsIterator()));
                }
            }
        }

    }
    
}
