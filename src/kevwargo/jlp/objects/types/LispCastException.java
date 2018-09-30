package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;

public class LispCastException extends LispException {

    public LispCastException(String fmt, Object ...args) {
        super(String.format(fmt, args));
    }

}
