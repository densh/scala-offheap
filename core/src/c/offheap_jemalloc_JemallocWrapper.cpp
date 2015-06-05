#include "offheap_jemalloc_JemallocWrapper.h"

#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT jlong JNICALL Java_offheap_jemalloc_JemallocWrapper_malloc_10 (JNIEnv *, jclass, jlong size) {
    return (jlong) je_malloc(static_cast<size_t>(size));
}

JNIEXPORT jlong JNICALL Java_offheap_jemalloc_JemallocWrapper_realloc_10
  (JNIEnv *, jclass, jlong, jlong);

JNIEXPORT void JNICALL Java_offheap_jemalloc_JemallocWrapper_free_10
  (JNIEnv *, jclass, jlong);

#ifdef __cplusplus
}
#endif
