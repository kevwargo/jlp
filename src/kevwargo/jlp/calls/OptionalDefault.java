package kevwargo.jlp.calls;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispRuntime;

public class OptionalDefault {

    private String name;
    private LispObject defaultValue;

    public OptionalDefault(LispObject arg, LispRuntime runtime) throws LispException {
        if (arg.isInstance(LispSymbol.TYPE)) {
            name = ((LispSymbol) arg.cast(LispSymbol.TYPE)).getName();
        } else if (arg.isInstance(LispList.TYPE)) {
            LispList list = (LispList) arg.cast(LispList.TYPE);
            if (list.size() != 2) {
                throw new LispException("Invalid optional default list: %s", list.toString());
            }

            name = ((LispSymbol) list.get(0).cast(LispSymbol.TYPE)).getName();
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
