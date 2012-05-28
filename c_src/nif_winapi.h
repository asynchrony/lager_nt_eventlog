#ifndef NIF_WINAPI_H
#define NIF_WINAPI_H

#include <windows.h>
#include <erl_nif.h>

#define NIF_FUNC(func, arity) {#func, arity, func}

#define NIF_GET_INT(env, term, int_ptr) \
    if (! enif_get_int(env, term, int_ptr)) \
        return enif_make_badarg(env);

#define NIF_GET_STRING(env, term, buf, buf_size) \
    if (0 >= enif_get_string(env, term, buf, buf_size, ERL_NIF_LATIN1)) \
        return enif_make_badarg(env);

#define NIF_ERR(expr) do { \
            ERL_NIF_TERM nif_err__temp = (expr); \
            if (nif_err__temp) return nif_err__temp; \
        } while (0);

#define WINAPI_ERR(env, expr) do { \
            LONG _err__temp = (expr); \
            if (_err__temp != ERROR_SUCCESS) \
                return make_error_tuple(env, _err__temp, \
                        __FILE__, __LINE__); \
        } while (0);

#define WINAPI_LASTERR(env, expr) do { \
            if (! (expr)) \
                return make_error_tuple(env, GetLastError(), \
                        __FILE__, __LINE__); \
        } while(0);

#define NIF_SUCCESS (ERL_NIF_TERM) NULL

/* {error, "foo.c", 166, 16#8009000F, "Object already exists."} */
ERL_NIF_TERM
make_error_tuple(ErlNifEnv *env, DWORD error_code, const char* filename,
                 const unsigned int line_number);

LPTSTR
string_format(LPCTSTR format, ...);

ERL_NIF_TERM
alloc_string_from_term(ErlNifEnv *env, ERL_NIF_TERM term, char **string);

#endif /* ndef NIF_WINAPI_H */
