package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;

public class LFAppend extends LispFunction {

    public LFAppend() {
        super(LispType.FUNCTION, "append", new FormalArguments().rest("args"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        ArrayList<LispObject> result = new ArrayList<LispObject>();
        Iterator<LispObject> argsIterator =
                ((LispList) arguments.get("args").cast(LispType.LIST)).iterator();
        while (argsIterator.hasNext()) {
            LispObject list = argsIterator.next().cast(LispType.LIST);
            Iterator<LispObject> listIterator = ((LispList) list).iterator();
            while (listIterator.hasNext()) {
                result.add(listIterator.next());
            }
        }
        if (result.isEmpty()) {
            return LispNil.NIL;
        }
        return new LispList(result);
    }
}
