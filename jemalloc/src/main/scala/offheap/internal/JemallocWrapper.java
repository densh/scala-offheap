package scala.offheap.internal;

public final class JemallocWrapper {
    public static final boolean Is32BitWordSize;
    public static final long Max32BitAllocationRequestSize = (1L << 32) - 1;

    static {
        JNILoader.ensureLoaded();
        Is32BitWordSize = is32BitWordSize_0();
    }

    public static long malloc(long size) {
        return malloc_0(size);
    }

    public static long realloc(long address, long newSize) {
        return realloc_0(address, newSize);
    }

    public static void free(long address) {
        free_0(address);
    }

    private static native boolean is32BitWordSize_0();
    private static native long malloc_0(long size);
    private static native long realloc_0(long address, long newSize);
    private static native void free_0(long address);
}

