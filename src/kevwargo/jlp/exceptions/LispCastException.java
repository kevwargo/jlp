package kevwargo.jlp.exceptions;

public class LispCastException extends LispException {

    public LispCastException(String fmt, Object ...args) {
        super(String.format(fmt, args));
    }

}
