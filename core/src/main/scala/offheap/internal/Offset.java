package offheap.internal;

import offheap.internal.Unsafer;
import offheap.internal.CASLinkedRegion;
import offheap.internal.CASLinkedPage;
import offheap.internal.CASLinkedPagePool$;
import sun.misc.Unsafe;

public class Offset {
    static {
        Unsafe unsafe = Unsafer.unsafe;
        try {
            PageOffset = unsafe.fieldOffset(CASLinkedPage.class.getDeclaredField("offset"));
            RegionPage = unsafe.fieldOffset(CASLinkedRegion.class.getDeclaredField("page"));
            PoolChunk = unsafe.fieldOffset(CASLinkedPagePool$.class.getDeclaredField("chunk"));
            PoolPage = unsafe.fieldOffset(CASLinkedPagePool$.class.getDeclaredField("page"));
        } catch (NoSuchFieldException e) {
            System.out.println("No such field");
        }
    }
    static long PageOffset;
    static long RegionPage;
    static long PoolChunk;
    static long PoolPage;
}
