package offheap.internal;

public class CheckedHolder {
    public static final boolean CHECKED;
    static {
        CHECKED = !System.getProperties().containsKey("offheap.unchecked");
    }
}
