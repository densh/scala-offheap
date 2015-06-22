#include "scala_offheap_internal_JemallocWrapper.h"
#include <bits/wordsize.h>

#include <iostream>
#ifdef __cplusplus
extern "C" {
#endif

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)

static constexpr auto SizeMismatch      = -1;
static constexpr auto AllocationFailure = -2;

#define sizeCheck(x)            if (unlikely((size_t) x != x)) { return SizeMismatch; } 
#define resultOfAllocation(x)   if (likely(x != NULL)) { return (jlong) x; } else { return AllocationFailure; }

JNIEXPORT jboolean JNICALL Java_scala_offheap_internal_JemallocWrapper_is32BitWordSize_10 (JNIEnv *, jclass) {
    return __WORDSIZE == 32;
}

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_malloc_10 (JNIEnv *, jclass, jlong size) {
    sizeCheck(size);
    const auto result = je_malloc(static_cast<size_t>(size));
    resultOfAllocation(result);
}

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_realloc_10 (JNIEnv *, jclass, jlong address, jlong newSize) {
    sizeCheck(newSize);
    const auto result = je_realloc((void *) address, static_cast<size_t>(newSize));
    resultOfAllocation(result);
}

JNIEXPORT void JNICALL Java_scala_offheap_internal_JemallocWrapper_free_10 (JNIEnv *, jclass, jlong address) {
    je_free((void *) address);
}

#ifdef __cplusplus
}
#endif
