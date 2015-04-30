package offheap;

import static offheap.internal.UnsafeHolder.UNSAFE;
import static offheap.internal.CheckedHolder.CHECKED;

public final class Sanitizer {
    static final long ID_MASK = Long.MAX_VALUE << 48;
    static final long ADDR_MASK = Long.MAX_VALUE >> 16;

    public static long pack(short id, long addr) {
        return ((long) id << 48) | addr;
    }
    public static short unpackId(long addr) {
        return (short) ((addr & ID_MASK) >> 48);
    }
    public static long unpackAddr(long addr) {
        return addr & ADDR_MASK;
    }

    public static long validate(long addr) throws InaccessibleMemoryException {
        if (CHECKED) {
            if (!accessible(unpackId(addr)))
                throw new InaccessibleMemoryException("");
            return unpackAddr(addr);
        } else {
            return addr;
        }
    }

    static final short MAX_ID = Short.MAX_VALUE;
    static final long arr = UNSAFE.allocateMemory(MAX_ID);
    static int last = 0;
    static int count = 0;

    static int advance(int last) {
        int inc = last + 1;
        if (inc < MAX_ID)
            return inc;
        else
            return 0;
    }

    public synchronized static short register() {
        while (UNSAFE.getByte(arr + last) != 0)
            last = advance(last);
        short res = (short) last;
        UNSAFE.putByteVolatile(null, arr + last, (byte) 1);
        last = advance(last);
        count += 1;
        return res;
    }

    public synchronized static void unregister(short id) {
        count -= 1;
        UNSAFE.putByteVolatile(null, arr + id, (byte) 0);
    }

    public static boolean accessible(short id) {
        return UNSAFE.getByteVolatile(null, arr + id) == 1;
    }
}
