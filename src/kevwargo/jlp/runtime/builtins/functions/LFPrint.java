package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.objects.wrappers.LispJavaObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Iterator;

public class LFPrint extends LispFunction {

    public static final String NAME = "print";
    public static final String ARG_OBJECTS = "objects";
    public static final String ARG_STREAM = "stream";

    public LFPrint() {
        super(
                LispFunction.FUNCTION_TYPE,
                NAME,
                new CallArgs().rest(ARG_OBJECTS).key(ARG_STREAM, LispNil.NIL));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        PrintStream out;
        if (args.get(ARG_STREAM) != LispNil.NIL) {
            LispJavaObject streamObj =
                    (LispJavaObject) args.get(ARG_STREAM).cast(LispJavaObject.TYPE);
            if (!(streamObj.getObject() instanceof OutputStream)) {
                throw new LispException(
                        "'%s' is not a java.io.OutputStream", streamObj.getObject());
            }
            out = new PrintStream((OutputStream) streamObj.getObject());
        } else {
            out = new PrintStream(runtime.getOut());
        }

        Iterator<LispObject> it = ((LispList) args.get(ARG_OBJECTS)).iterator();

        if (it.hasNext()) {
            out.print(it.next().toString());
        }
        while (it.hasNext()) {
            out.print(' ');
            out.print(it.next().toString());
        }
        out.println();
        return LispNil.NIL;
    }
}
