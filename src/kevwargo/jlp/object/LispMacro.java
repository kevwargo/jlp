package kevwargo.jlp.object;

public abstract class LispMacro extends LispFunction {

    public LispMacro(String name, String formalArguments[], boolean allowRest) {
        super(name, formalArguments, allowRest);
        evalArgs = false;
    }
    
}
