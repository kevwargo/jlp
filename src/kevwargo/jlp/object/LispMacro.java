package kevwargo.jlp.object;

public abstract class LispMacro extends LispBuiltinFunction {

    public LispMacro(String name, String formalArguments[], boolean allowRest) {
        super(name, formalArguments, allowRest);
        evalArgs = false;
    }
    
}
