#include "scala_offheap_internal_JemallocWrapper.h"

#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_malloc_10 (JNIEnv *, jclass, jlong size) {
    const auto result = je_malloc(static_cast<size_t>(size));

    return result == NULL ? -1 : (jlong) result;
}

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_realloc_10 (JNIEnv *, jclass, jlong address, jlong newSize) {
    const auto result = je_realloc((void *) address, static_cast<size_t>(newSize));

    return result == NULL ? -1 : (jlong) result;
}

JNIEXPORT void JNICALL Java_scala_offheap_internal_JemallocWrapper_free_10 (JNIEnv *, jclass, jlong address) {
    je_free((void *) address);
}

#ifdef __cplusplus
}
#endif
