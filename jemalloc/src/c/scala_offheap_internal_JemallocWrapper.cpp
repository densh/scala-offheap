#include "scala_offheap_internal_JemallocWrapper.h"

#include <bits/wordsize.h>
#include <iostream>
#include <gnu/libc-version.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)

static const jlong SizeMismatch      = -1;
static const jlong AllocationFailure = -2;

#define sizeCheck(x)            if (unlikely((size_t) x != x)) { return SizeMismatch; } 
#define resultOfAllocation(x)   if (likely(x != NULL)) { return (jlong) x; } else { return AllocationFailure; }

static const uint64_t TestValue = (uint64_t) INT64_MAX + 25;

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_generateUnsignedLong_10 (JNIEnv *, jclass) {
    return (jlong) TestValue;
}

JNIEXPORT jboolean JNICALL Java_scala_offheap_internal_JemallocWrapper_verifyUnsignedLong_10 (JNIEnv *, jclass, jlong value) {
    if (value >= 0) {
        return false;
    }

    uintptr_t result = (uintptr_t) value;

    return result == TestValue;
}

JNIEXPORT jboolean JNICALL Java_scala_offheap_internal_JemallocWrapper_is32BitWordSize_10 (JNIEnv *, jclass) {
    std::cout << "GLIB Version: " << gnu_get_libc_version() << std::endl;
   
    return __WORDSIZE == 32;
}

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_malloc_10 (JNIEnv *, jclass, jlong size) {
    sizeCheck(size);
    const void* result = je_malloc(static_cast<size_t>(size));
    resultOfAllocation(result);
}

JNIEXPORT jlong JNICALL Java_scala_offheap_internal_JemallocWrapper_realloc_10 (JNIEnv *, jclass, jlong address, jlong newSize) {
    sizeCheck(newSize);
    uintptr_t validAddress = (uintptr_t) address;
    const void* result = je_realloc((void *) validAddress, static_cast<size_t>(newSize));
    resultOfAllocation(result);
}

JNIEXPORT void JNICALL Java_scala_offheap_internal_JemallocWrapper_free_10 (JNIEnv *, jclass, jlong address) {
    uintptr_t validAddress = (uintptr_t) address;
    je_free((void *) validAddress);
}

#ifdef __cplusplus
}
#endif
