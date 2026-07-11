#pragma once

#include <cstdint>
#if defined(_MSC_VER)
#include <intrin.h>
#endif

#ifdef USE_PEXT
#include <immintrin.h>
#endif


#ifdef __linux__
#include <stdalign.h>


#define align __attribute__((aligned(64)))

#define inline_always inline __attribute__((always_inline))
#else
#define inline_always __forceinline
#endif

/*
 * Clang allows us to specify inline always on recursive functions,
 * GCC does not.
 */
#ifdef __clang__
#define inline_hint inline __attribute__((always_inline))
#elif defined(__GNUC__)
#define inline_hint inline
#else
#define inline_hint inline
#endif

#ifdef __clang__
#define cache_align __attribute__((aligned(64)))
#else
#define cache_align __declspec(align(64))
#endif

#if defined(__GNUC__) || defined(__clang__)
#define PREFETCH_DEFAULT(xs) __builtin_prefetch((xs), 0, 0)
#define PREFETCH_SOON(xs) __builtin_prefetch((xs), 0, 1)
#else
#define PREFETCH_DEFAULT(xs) (void)(xs)
#define PREFETCH_SOON(xs) (void)(xs)
#endif
