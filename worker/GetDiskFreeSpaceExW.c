#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#if defined(_WIN32)

#include <windef.h>
#include <fileapi.h>

#include <caml/misc.h>
#include <caml/osdeps.h>
#include <caml/fail.h>

CAMLprim value
stub_GetDiskFreeSpaceExW(value directoryName)
{
    CAMLparam1(directoryName);
    CAMLlocal1(pair);
    BOOL found = FALSE;
    ULONGLONG freeBytes = 0ULL, totalNumber = 0ULL;
    char_os *osDirectoryName =
        caml_stat_strdup_to_os(String_val(directoryName));

    found = GetDiskFreeSpaceExW(osDirectoryName,
                                (PULARGE_INTEGER) &freeBytes,
                                (PULARGE_INTEGER) &totalNumber,
                                (PULARGE_INTEGER) NULL)
        && freeBytes <= LLONG_MAX && totalNumber <= LLONG_MAX;
    caml_stat_free(osDirectoryName);

    if(!found)
        caml_raise_not_found();

    pair = caml_alloc_small(16, 0);
    Field(pair, 0) = caml_copy_int64(freeBytes);
    Field(pair, 1) = caml_copy_int64(totalNumber);
    CAMLreturn(pair);
}

#else

CAMLprim value
stub_GetDiskFreeSpaceExW(value directoryName)
{
    CAMLparam1(directoryName);
    CAMLlocal1(pair);
    pair = caml_alloc_small(16, 0);
    Field(pair, 0) = caml_copy_int64(0ULL);
    Field(pair, 1) = caml_copy_int64(0ULL);
    CAMLreturn(pair);
}

#endif
