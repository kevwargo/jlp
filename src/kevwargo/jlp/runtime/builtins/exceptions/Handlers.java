package kevwargo.jlp.runtime.builtins.exceptions;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.objects.wrappers.LispJavaClass;
import kevwargo.jlp.objects.wrappers.LispJavaObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Handlers {

    private static final LispSymbol symFinally = new LispSymbol("finally");
    private List<LispList> exceptClauses;
    private LispList finallyClause;
    private LispRuntime runtime;

    public Handlers(LispList clauses, LispRuntime runtime) throws LispException {
        this.runtime = runtime;
        exceptClauses = new ArrayList<LispList>();
        Iterator<LispObject> it = clauses.contents().iterator();
        while (it.hasNext()) {
            LispList clause = (LispList) it.next().cast(LispList.TYPE);
            if (clause.contents().isEmpty()) {
                throw new LispException("Empty try-except-finally clauses are not allowed");
            }

            if (!it.hasNext() && isFinally(clause)) {
                finallyClause =
                        new LispList(clause.contents().subList(1, clause.contents().size()));
            } else {
                exceptClauses.add(clause);
            }
        }
    }

    public Handler findHandler(Throwable throwable) throws LispException {
        for (LispList clause : exceptClauses) {
            Iterator<LispObject> it = clause.contents().iterator();
            LispObject exc = it.next();

            if (exc.isInstance(LispSymbol.TYPE)) {
                if (exceptMatches(throwable, exc)) {
                    return new Handler(new LispList(it), runtime);
                }
            } else if (exc.isInstance(LispList.TYPE)) {
                LispList binding = (LispList) exc.cast(LispList.TYPE);
                if (binding.contents().size() != 2) {
                    throw new LispException("%s does not match the (EXC VAR) form", binding.repr());
                }
                String var = ((LispSymbol) binding.get(1).cast(LispSymbol.TYPE)).getName();

                if (exceptMatches(throwable, binding.get(0))) {
                    Layer layer = new Layer();
                    layer.put(var, LispBaseObject.wrap(throwable));
                    return new Handler(new LispList(it), runtime.with(layer));
                }
            } else {
                throw new LispCastException(
                        "The beginning of each 'except' clause must be either a symbol or a (EXC"
                                + " VAR) list, not %s",
                        exc.repr());
            }
        }

        return null;
    }

    public void runFinally(LispRuntime runtime) throws LispException {
        if (finallyClause != null) {
            for (LispObject form : finallyClause) {
                form.eval(runtime);
            }
        }
    }

    private boolean exceptMatches(Throwable throwable, LispObject exc) throws LispException {
        LispObject excClassWrapper = exc.eval(runtime).cast(LispJavaObject.TYPE);
        if (!(excClassWrapper instanceof LispJavaClass)) {
            throw new LispException("%s is not a Java class", excClassWrapper.repr());
        }

        Class<?> excClass = ((LispJavaClass) excClassWrapper).getWrappedClass();
        return excClass.isInstance(throwable);
    }

    private static boolean isFinally(LispList clause) {
        return clause.contents().get(0).equals(symFinally);
    }
}
