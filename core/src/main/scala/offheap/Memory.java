package scala.offheap;

import static scala.offheap.internal.SunMisc.UNSAFE;

public class Memory {
    public static boolean isAvailable() {
        return null != UNSAFE;
    }
    public static void copy(long from, long to, long size) {
        UNSAFE.copyMemory((from), (to), size);
    }
    public static void zero(long addr, long size) {
        UNSAFE.setMemory((addr), size, (byte) 0);
    }
    public static char getChar(long addr) {
        return UNSAFE.getChar((addr));
    }
    public static byte getByte(long addr) {
        return UNSAFE.getByte((addr));
    }
    public static short getShort(long addr) {
        return UNSAFE.getShort((addr));
    }
    public static int getInt(long addr) {
        return UNSAFE.getInt((addr));
    }
    public static long getLong(long addr) {
        return UNSAFE.getLong((addr));
    }
    public static float getFloat(long addr) {
        return UNSAFE.getFloat((addr));
    }
    public static double getDouble(long addr) {
        return UNSAFE.getDouble((addr));
    }
    public static void putChar(long addr, char value) {
        UNSAFE.putChar((addr), value);
    }
    public static void putByte(long addr, byte value) {
        UNSAFE.putByte((addr), value);
    }
    public static void putShort(long addr, short value) {
        UNSAFE.putShort((addr), value);
    }
    public static void putInt(long addr, int value) {
        UNSAFE.putInt((addr), value);
    }
    public static void putLong(long addr, long value) {
        UNSAFE.putLong((addr), value);
    }
    public static void putFloat(long addr, float value) {
        UNSAFE.putFloat((addr), value);
    }
    public static void putDouble(long addr, double value) {
        UNSAFE.putDouble((addr), value);
    }
}
