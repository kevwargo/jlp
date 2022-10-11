package kevwargo.jlp.calls;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;

public class OptionalDefault {

    private String name;
    private LispObject defaultValue;

    public OptionalDefault(LispObject arg, LispRuntime runtime) throws LispException {
        if (arg.isInstance(LispType.SYMBOL)) {
            name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();
        } else if (arg.isInstance(LispType.LIST)) {
            LispList list = (LispList) arg.cast(LispType.LIST);
            if (list.size() != 2) {
                throw new LispException("Invalid optional default list: %s", list.toString());
            }

            name = ((LispSymbol) list.get(0).cast(LispType.SYMBOL)).getName();
            defaultValue = list.get(1).eval(runtime);
        } else {
            throw new LispException("expected symbol or list, not '%s'", arg.getType().getName());
        }
    }

    public String getName() {
        return name;
    }

    public LispObject getDefault() {
        return defaultValue;
    }
}
