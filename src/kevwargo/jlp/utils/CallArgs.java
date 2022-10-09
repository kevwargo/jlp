package kevwargo.jlp.utils;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class CallArgs {

    private List<String> positional;
    private String rest;
    private List<String> optional;
    private boolean ignore;

    private static CallArgs ignored = new CallArgs();

    static {
        ignored.ignore = true;
    }

    public CallArgs(String... args) {
        positional = new ArrayList<String>(args.length);
        for (String arg : args) {
            positional.add(arg);
        }
        optional = new ArrayList<String>();
    }

    public CallArgs(LispList lambdaExpr) throws LispException {
        positional = new ArrayList<String>();
        optional = new ArrayList<String>();

        boolean restExpected = false;

        for (LispObject arg : lambdaExpr) {
            String name = ((LispSymbol) arg.cast(LispType.SYMBOL)).getName();

            if (restExpected) {
                if (rest != null) {
                    throw new LispException("&rest declaration must be the last");
                }
                rest = name;
            } else if (name.equals("&rest")) {
                restExpected = true;
            } else {
                positional.add(name);
            }
        }
    }

    public static CallArgs ignored() {
        return ignored;
    }

    public CallArgs clone() {
        CallArgs clone = new CallArgs();
        clone.positional = positional;
        clone.optional = optional;
        clone.rest = rest;
        return clone;
    }

    public String pop() {
        return positional.remove(0);
    }

    public List<String> opt() {
        return optional;
    }

    public CallArgs opt(String arg) {
        optional.add(arg);
        return this;
    }

    public String rest() {
        return rest;
    }

    public CallArgs rest(String rest) {
        this.rest = rest;
        return this;
    }

    public LispNamespace.Layer apply(LispList args) throws LispException {
        if (ignore) {
            return null;
        }

        if (args.size() < positional.size()) {
            throw new LispException("Not enough arguments");
        }

        LispNamespace.Layer layer = new LispNamespace.Layer();

        Iterator<LispObject> it = args.iterator();
        for (String arg : positional) {
            layer.put(arg, it.next());
        }

        for (int i = 0; i < optional.size() && it.hasNext(); i++) {
            layer.put(optional.get(i), it.next());
        }

        if (rest == null) {
            if (it.hasNext()) {
                throw new LispException("Too many arguments");
            }
            return layer;
        }

        layer.put(rest, new LispList(it));
        return layer;
    }
}
