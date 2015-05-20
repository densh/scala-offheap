package offheap.internal;

import sun.misc.Unsafe;
import java.lang.reflect.Field;
import static offheap.internal.Sanitizer.validate;

public class Memory {
    public static final Unsafe UNSAFE;
    static {
        Unsafe value = null;
        try {
            value = Unsafe.getUnsafe();
        } catch (SecurityException e) {
            try {
                Field f = Unsafe.class.getDeclaredField("theUnsafe");
                f.setAccessible(true);
                value = (Unsafe) f.get(null);
            } catch (NoSuchFieldException nsfe) {
            } catch (IllegalAccessException iae) {
            }
        }
        UNSAFE = value;
    }
    public static long allocate(long size) {
        return UNSAFE.allocateMemory(size);
    }
    public static void copy(long from, long to, long size) {
        UNSAFE.copyMemory(validate(from), validate(to), size);
    }
    public static void zero(long addr, long size) {
        UNSAFE.setMemory(validate(addr), size, (byte) 0);
    }
    public static char getChar(long addr) {
        return UNSAFE.getChar(validate(addr));
    }
    public static byte getByte(long addr) {
        return UNSAFE.getByte(validate(addr));
    }
    public static short getShort(long addr) {
        return UNSAFE.getShort(validate(addr));
    }
    public static int getInt(long addr) {
        return UNSAFE.getInt(validate(addr));
    }
    public static long getLong(long addr) {
        return UNSAFE.getLong(validate(addr));
    }
    public static float getFloat(long addr) {
        return UNSAFE.getFloat(validate(addr));
    }
    public static double getDouble(long addr) {
        return UNSAFE.getDouble(validate(addr));
    }
    public static void putChar(long addr, char value) {
        UNSAFE.putChar(validate(addr), value);
    }
    public static void putByte(long addr, byte value) {
        UNSAFE.putByte(validate(addr), value);
    }
    public static void putShort(long addr, short value) {
        UNSAFE.putShort(validate(addr), value);
    }
    public static void putInt(long addr, int value) {
        UNSAFE.putInt(validate(addr), value);
    }
    public static void putLong(long addr, long value) {
        UNSAFE.putLong(validate(addr), value);
    }
    public static void putFloat(long addr, float value) {
        UNSAFE.putFloat(validate(addr), value);
    }
    public static void putDouble(long addr, double value) {
        UNSAFE.putDouble(validate(addr), value);
    }
}
