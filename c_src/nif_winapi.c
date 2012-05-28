#include "windows.h"
#include "nif_winapi.h"

ERL_NIF_TERM
make_error_tuple(ErlNifEnv *env, DWORD error_code, const char* filename,
                 const unsigned int line_number)
{
    LPTSTR message = NULL;
    ERL_NIF_TERM tuple;

    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_ALLOCATE_BUFFER,
                  0, error_code, 0, (LPTSTR)&message, 0, NULL);

    tuple = enif_make_tuple5(env, enif_make_atom(env, "error"),
                            enif_make_string(env, filename, ERL_NIF_LATIN1),
                            enif_make_uint(env, line_number),
                            enif_make_uint(env, (unsigned int)error_code),
                            enif_make_string(env, message, ERL_NIF_LATIN1));
    LocalFree(message);
    return tuple;
}

LPTSTR
string_format(LPCTSTR format, ...)
{
    LPTSTR output = NULL;
    va_list args = NULL;

    va_start(args, format);
    FormatMessage(FORMAT_MESSAGE_FROM_STRING |
                  FORMAT_MESSAGE_ALLOCATE_BUFFER,
                  format,
                  0,
                  0,
                  (LPTSTR)&output,
                  0,
                  &args);
    va_end(args);

    return output;
}

ERL_NIF_TERM
alloc_string_from_term(ErlNifEnv *env, ERL_NIF_TERM term, char **string)
{
    unsigned int string_len;

    if (! enif_get_list_length(env, term, &string_len))
        return enif_make_badarg(env);
    string_len += 1;

    WINAPI_LASTERR(env, *string = (char*) LocalAlloc(0, string_len));

    NIF_GET_STRING(env, term, *string, string_len);

    return NIF_SUCCESS;
}

