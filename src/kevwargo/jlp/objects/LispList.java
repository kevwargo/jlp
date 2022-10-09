package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class LispList extends LispBaseObject implements LispIterable {

    private List<LispObject> contents;
    private boolean special;

    public LispList(List<LispObject> contents, boolean special) {
        super(LispType.LIST);
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

    public Iterator<LispObject> iterator() {
        return contents.iterator();
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
        if (!callable.isInstance(LispType.MACRO)) {
            arglist = arglist.evalElements(runtime);
        }

        LispObject result = callable.call(runtime, callable.getCallArgs().apply(arglist));
        if (callable.isInstance(LispType.LISP_MACRO)) {
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
        sb.append("(");
        Iterator<LispObject> it = iterator();
        if (it.hasNext()) {
            sb.append(it.next().repr());
        }
        while (it.hasNext()) {
            sb.append(" ");
            sb.append(it.next().repr());
        }
        sb.append(")");
        return sb.toString();
    }

    public boolean bool() {
        return !isEmpty();
    }
}

class ListType extends LispType {

    private static final String ARG_ARGS = "args";

    ListType() {
        super("list", new LispType[] {OBJECT}, new CallArgs().rest(ARG_ARGS));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        return args.get(ARG_ARGS);
    }
}
