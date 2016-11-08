package kevwargo.jlp.object;

import java.util.HashMap;
import java.util.List;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;


public abstract class LispBuiltinFunction extends LispObject {

    private String[] formalArguments;
    private boolean allowRest;
    protected String name;
    protected HashMap<String, LispObject> arguments;
    protected LispObject[] rest;
    protected boolean evalArgs;

    public LispBuiltinFunction(String name, String[] formalArguments, boolean allowRest) {
        this.name = name;
        this.formalArguments = formalArguments;
        this.allowRest = allowRest;
        evalArgs = true;
    }

    public void setArguments(LispNamespace namespace, List<LispObject> arguments) throws LispException {
        int actualSize = arguments.size();
        int formalSize = formalArguments.length;
        if (actualSize < formalSize || actualSize > formalSize && !allowRest) {
            throw new LispException(
                    String.format(
                            "Wrong number of arguments for %s: %d (%d expected)",
                            name,
                            actualSize,
                            formalSize
                        )
                );
        }
        this.arguments = new HashMap<String, LispObject>();
        if (evalArgs) {
            for (int i = 0; i < formalSize; i++) {
                this.arguments.put(formalArguments[i], arguments.get(i).eval(namespace));
            }
        } else {
            for (int i = 0; i < formalSize; i++) {
                this.arguments.put(formalArguments[i], arguments.get(i));
            }
        }
        rest = new LispObject[actualSize - formalSize];
        if (evalArgs) {
            for (int i = formalSize; i < actualSize; i++) {
                rest[i - formalSize] = arguments.get(i).eval(namespace);
            }
        } else {
            for (int i = formalSize; i < actualSize; i++) {
                rest[i - formalSize] = arguments.get(i);
            }
        }
    }

}
