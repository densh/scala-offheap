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
            PoolChunk = unsafe.fieldOffset(CASLinkedRegion.class.getDeclaredField("chunk"));
            PoolPage = unsafe.fieldOffset(CASLinkedRegion.class.getDeclaredField("page"));
        } catch (NoSuchFieldException e) {
        }
    }
    static int PageOffset;
    static int RegionPage;
    static int PoolChunk;
    static int PoolPage;
}
