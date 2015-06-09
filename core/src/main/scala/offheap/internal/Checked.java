package scala.offheap.internal;

public class Checked {
    public static final boolean NULL;
    public static final boolean BOUNDS;
    public static final boolean MEMORY;
    static {
        boolean all = hasKey("offheap.unchecked.all");
        NULL        = !all && !hasKey("offheap.unchecked.null");
        BOUNDS      = !all && !hasKey("offheap.unchecked.bounds");
        MEMORY      = hasKey("offheap.checked.memory");
    }
    private static boolean hasKey(String key) {
        return System.getProperties().containsKey(key);
    }
}
