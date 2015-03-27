package offheap.internal;

public class Unsafer {
    static {
        try {
            unsafe = sun.misc.Unsafe.getUnsafe();
        } catch (SecurityException e) {
            try {
                java.lang.reflect.Field f = sun.misc.Unsafe.class.getDeclaredField("theUnsafe");
                f.setAccessible(true);
                unsafe = (sun.misc.Unsafe) f.get(null);
            } catch (NoSuchFieldException nsfe) {
            } catch (IllegalAccessException iae) {
            }
        }
    }
    public static sun.misc.Unsafe unsafe;
}
