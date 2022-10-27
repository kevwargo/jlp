package kevwargo.jlp.objects.collections;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.functions.LispCallable;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.iter.LispIterable;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

public class LispList extends LispBaseObject implements LispIterable {

    public static final LispType TYPE = new ListType();

    private List<LispObject> contents;
    private boolean special;

    public LispList(List<LispObject> contents, boolean special) {
        super(LispList.TYPE);
        this.contents = contents;
        this.special = special;
    }

    public LispList(List<LispObject> contents) {
        this(contents, false);
    }

    public LispList(boolean special) {
        this(new ArrayList<LispObject>(), special);
    }

    public LispList(Iterator<LispObject> contents) {
        this(false);
        while (contents.hasNext()) {
            this.contents.add(contents.next());
        }
    }

    public LispList() {
        this(new ArrayList<LispObject>(), false);
    }

    public List<LispObject> contents() {
        return contents;
    }

    public Iterator<LispObject> iterator() {
        return contents.iterator();
    }

    public ListIterator<LispObject> listIterator() {
        return contents.listIterator();
    }

    public LispList add(LispObject object) {
        contents.add(object);
        return this;
    }

    public LispList add(int index, LispObject object) {
        contents.add(index, object);
        return this;
    }

    public LispList addAll(LispList list) {
        contents.addAll(list.contents);
        return this;
    }

    public LispObject get(int index) {
        return this.contents.get(index);
    }

    public int size() {
        return contents.size();
    }

    public boolean isEmpty() {
        return contents.isEmpty();
    }

    public boolean isSpecial() {
        return special;
    }

    public LispObject eval(LispRuntime runtime) throws LispException {
        if (isEmpty()) {
            return this;
        }

        LispObject head = get(0).eval(runtime);
        if (!(head instanceof LispCallable)) {
            throw new LispException("Object '%s' is not callable", head.getType().getName());
        }

        LispList arglist = new LispList(contents.subList(1, size()));
        return arglist.applyCallable((LispCallable) head, runtime);
    }

    public LispObject applyCallable(LispCallable callable, LispRuntime runtime)
            throws LispException {
        LispList arglist = this;
        if (!callable.isInstance(LispFunction.MACRO_TYPE)) {
            arglist = arglist.evalElements(runtime);
        }

        LispObject result = callable.call(runtime, callable.getCallArgs().apply(arglist));
        if (callable.isInstance(LispFunction.LISP_MACRO_TYPE)) {
            result = result.eval(runtime);
        }

        return result;
    }

    private LispList evalElements(LispRuntime runtime) throws LispException {
        LispList list = new LispList();
        for (LispObject element : contents) {
            list.add(element.eval(runtime));
        }

        return list;
    }

    public String repr() {
        StringBuffer sb = new StringBuffer();
        Iterator<LispObject> it = iterator();

        sb.append("(");

        if (it.hasNext()) {
            sb.append(it.next().repr());
        }
        while (it.hasNext()) {
            sb.append(" ");
            sb.append(it.next().repr());
        }

        return sb.append(")").toString();
    }

    public boolean bool() {
        return !isEmpty();
    }
}

class ListType extends LispType {

    private static final String ARG_ARGS = "args";

    ListType() {
        super("list", new LispType[] {LispBaseObject.TYPE}, new CallArgs().rest(ARG_ARGS));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return args.get(ARG_ARGS);
    }
}
