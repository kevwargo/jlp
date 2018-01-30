package kevwargo.jlp.objects;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class LispList extends LispObject implements Iterable<LispObject> {

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

    public int size() {
        return contents.size();
    }

    public boolean isSpecial() {
        return special;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        Iterator<LispObject> iterator = iterator();
        if (! iterator.hasNext()) {
            return LispBool.FALSE;
        }
        LispObject function = iterator.next().eval(namespace);
        boolean isMacro = function.isInstance(LispType.MACRO);
        ArgumentsIterator args = new ArgumentsIterator(iterator, isMacro ? null : namespace, contents.size() - 1);
        return function.call(namespace, args);
    }

    public String repr() {
        StringBuffer sb = new StringBuffer();
        sb.append("(");
        Iterator<LispObject> iterator = iterator();
        if (iterator.hasNext()) {
            sb.append(iterator.next().repr());
        }
        while (iterator.hasNext()) {
            sb.append(" ");
            sb.append(iterator.next().repr());
        }
        sb.append(")");
        return sb.toString();
    }
}







