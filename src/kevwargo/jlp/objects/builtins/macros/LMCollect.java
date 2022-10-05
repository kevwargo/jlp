package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

import java.util.HashMap;
import java.util.Map;

public class LMCollect extends LispFunction {

    public LMCollect() {
        super(LispType.MACRO, "collect", new FormalArguments().rest("body"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        LispList body = (LispList) arguments.get("body");
        Map<String, LispObject> map = new HashMap<String, LispObject>();
        LispList result = new LispList();
        map.put("yield", new YieldFunction(result, false));
        map.put("yield-from", new YieldFunction(result, true));
        LispNamespace bodyNamespace = namespace.prepend(map);
        for (LispObject form : body) {
            form.eval(bodyNamespace);
        }
        return result;
    }

    private class YieldFunction extends LispFunction {

        private LispList result;
        private boolean from;

        public YieldFunction(LispList result, boolean from) {
            super(LispType.FUNCTION, "yield" + (from ? "-from" : ""), new FormalArguments("obj"));
            this.result = result;
            this.from = from;
        }

        protected LispObject callInternal(
                LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            LispObject obj = arguments.get("obj");
            if (from) {
                LispList list = (LispList) obj.cast(LispType.LIST);
                for (LispObject o : list) {
                    result.add(o);
                }
            } else {
                result.add(obj);
            }
            return obj;
        }
    }
}
