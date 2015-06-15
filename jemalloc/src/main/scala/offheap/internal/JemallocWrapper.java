package scala.offheap.internal;

public final class JemallocWrapper {
    static {
        JNILoader.ensureLoaded();
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

    private static native long malloc_0(long size);
    private static native long realloc_0(long address, long newSize);
    private static native void free_0(long address);
}

