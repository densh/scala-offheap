package offheap.internal;

import sun.misc.Unsafe;
import java.lang.reflect.Field;

public class Unsafer {
    static {
        try {
            unsafe = Unsafe.getUnsafe();
        } catch (SecurityException e) {
            try {
                Field f = Unsafe.class.getDeclaredField("theUnsafe");
                f.setAccessible(true);
                unsafe = (Unsafe) f.get(null);
            } catch (NoSuchFieldException nsfe) {
            } catch (IllegalAccessException iae) {
            }
        }
    }
    static Unsafe unsafe;
}
