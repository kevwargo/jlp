package kevwargo.jlp.objects.builtins.javareflect;

class DefaultClassLoader {

    static Class<?> load(String name) throws ClassNotFoundException {
        try {
            return Class.forName(name);
        } catch (ClassNotFoundException e) {
            return Class.forName("java.lang." + name);
        }
    }

}
