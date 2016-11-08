package kevwargo.jlp.objects;

public abstract class LispBuiltinMacro extends LispBuiltinFunction {

    public LispBuiltinMacro(String name, String formalArguments[], boolean allowRest) {
        super(name, formalArguments, allowRest);
        evalArgs = false;
    }
    
}
