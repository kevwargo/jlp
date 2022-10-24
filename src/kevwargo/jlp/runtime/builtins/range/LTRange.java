package kevwargo.jlp.runtime.builtins.range;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBaseObject;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispIterable;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class LTRange extends LispType {

    public static final String NAME = "range";
    public static final String ARG_FROM = "from";
    public static final String ARG_TO = "to";
    public static final String ARG_STEP = "step";

    private static final CallArgs args = new CallArgs(ARG_FROM, ARG_TO).opt(ARG_STEP);

    public LTRange() {
        super(NAME, new LispType[] {LispType.OBJECT}, args);
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        long from = ((LispInt) args.get(ARG_FROM).cast(INT)).getValue();
        long to = ((LispInt) args.get(ARG_TO).cast(INT)).getValue();
        long step = 1;
        if (args.get(ARG_STEP) != null) {
            step = ((LispInt) args.get(ARG_STEP).cast(INT)).getValue();
        }

        return new Range(this, from, to, step);
    }
}

class Range extends LispBaseObject implements LispIterable {
    private long from;
    private long to;
    private long step;

    Range(LTRange type, long from, long to, long step) {
        super(type);
        this.from = from;
        this.to = to;
        this.step = step;
    }

    public Iterator<LispObject> iterator() {
        return new RangeIterator(from, to, step);
    }
}

class RangeIterator extends LispIterator {

    private long to;
    private long step;
    private long counter;

    RangeIterator(long from, long to, long step) {
        this.to = to;
        this.step = step;
        this.counter = from;
    }

    public boolean hasNext() {
        if (step == 0) {
            return false;
        }
        if ((to - counter) * step < 0) {
            return false;
        }
        return counter != to;
    }

    public LispObject next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }

        LispInt result = new LispInt(counter);
        counter += step;
        return result;
    }
}
