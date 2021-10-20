package kevwargo.jlp;

public class LispCastException extends LispException {

    public LispCastException(String fmt, Object ...args) {
        super(String.format(fmt, args));
    }

}
