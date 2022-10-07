package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;

public interface LispObject {

    public LispType getType();

    public boolean isInstance(LispType type);

    public LispObject cast(LispType type) throws LispCastException;

    public LispObject eval(LispRuntime runtime) throws LispException;

    public LispObject getAttr(String name);

    public void setAttr(String name, LispObject value) throws LispException;

    public String repr();

    public Object format();

    public Object getJavaObject();

    public Class<?> getJavaClass();
}
