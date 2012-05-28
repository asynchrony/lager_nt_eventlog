#include "nif_winapi.h"
#include "lager_categories.h"
#include "registry_helpers.h"

#define NT_EVENTLOG_NIF_VSN (100)

const DWORD event_types_supported = EVENTLOG_ERROR_TYPE |
                                    EVENTLOG_WARNING_TYPE |
                                    EVENTLOG_INFORMATION_TYPE |
                                    EVENTLOG_AUDIT_SUCCESS |
                                    EVENTLOG_AUDIT_FAILURE;
const DWORD category_count = 9; /* number of categories in the MC file */
const char *registry_key_format =
    "SYSTEM\\CurrentControlSet\\Services\\EventLog\\Application\\%1";
ERL_NIF_TERM atom_ok;

ERL_NIF_TERM
register_event_source(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
deregister_event_source(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM
report_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ErlNifFunc nif_funcs[] = {
    NIF_FUNC(register_event_source, 1),
    NIF_FUNC(deregister_event_source, 1),
    NIF_FUNC(report_event, 3)
};

int
load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    int vsn;

    if ( (! enif_get_int(env, load_info, &vsn)) ||
            (NT_EVENTLOG_NIF_VSN != vsn))
        return -1;

    atom_ok = enif_make_atom(env, "ok");

    *priv_data = NULL;
    return 0;
}

ERL_NIF_INIT(nt_eventlog, nif_funcs, load, NULL, NULL, NULL)

ERL_NIF_TERM
get_path_to_this_dll(ErlNifEnv *env, LPTSTR buffer, DWORD buffer_size)
{
    HINSTANCE module_handle = NULL;
    DWORD path_length;

    /* Get handle to this DLL using address of module-local memory */
    WINAPI_LASTERR(env,
            GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
                              GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                              (LPCTSTR)&nif_funcs,
                              &module_handle));
    /* Get full path to this DLL */
    path_length = GetModuleFileName(module_handle,
                                    buffer,
                                    buffer_size);
    buffer[path_length] = '\0';

    return NIF_SUCCESS;
}

/*
 * Convert Lager priority to an EventLog type. The Lager application
 * supports 8 defined priorites, but the NT EventLog only knows
 * 3 event types of interest to us: ERROR, WARNING, and INFO.
 */
WORD
get_type(int priority)
{
    return (priority > LAGER_INFO)     ? EVENTLOG_SUCCESS :
           (priority > LAGER_WARNING)  ? EVENTLOG_INFORMATION_TYPE :
           (priority > LAGER_ERROR)    ? EVENTLOG_WARNING_TYPE :
                                         EVENTLOG_ERROR_TYPE;
    // EVENTLOG_AUDIT_SUCCESS
    // EVENTLOG_AUDIT_FAILURE
}

ERL_NIF_TERM
initialize_event_log_registry_key(ErlNifEnv *env, HKEY key)
{
    char path_to_this_dll[_MAX_PATH];

    NIF_ERR( get_path_to_this_dll(env, path_to_this_dll, _MAX_PATH -1) );

    WINAPI_ERR(env,
            set_registry_string_value(key, "EventMessageFile",
                                      path_to_this_dll));
    WINAPI_ERR(env,
            set_registry_string_value(key, "CategoryMessageFile",
                                      path_to_this_dll));
    WINAPI_ERR(env,
            set_registry_dword_value(key, "TypesSupported",
                                     &event_types_supported));
    WINAPI_ERR(env,
            set_registry_dword_value(key, "CategoryCount",
                                     &category_count));

    return NIF_SUCCESS;
}

ERL_NIF_TERM
add_registry_info(ErlNifEnv *env, char *event_source)
{
    DWORD disposition;
    HKEY key;
    char *subkey;

    WINAPI_LASTERR(env,
            subkey = string_format(registry_key_format, event_source));
    WINAPI_ERR(env,
            open_registry_key(HKEY_LOCAL_MACHINE, subkey, &key, &disposition));
    LocalFree(subkey);

    if (disposition == REG_CREATED_NEW_KEY)
        NIF_ERR( initialize_event_log_registry_key(env, key) );

    WINAPI_ERR(env, RegCloseKey(key));

    return NIF_SUCCESS;
}

ERL_NIF_TERM
register_event_source(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char event_source[_MAX_PATH];
    HANDLE handle;

    NIF_GET_STRING(env, argv[0], event_source, _MAX_PATH);

    NIF_ERR( add_registry_info(env, event_source) );

    WINAPI_LASTERR(env, handle = RegisterEventSource(NULL, event_source));

    return enif_make_tuple2(env, atom_ok, enif_make_int(env, (int)handle));
}

ERL_NIF_TERM
report_event(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HANDLE handle;
    int priority;
    char* message_text = NULL;

    NIF_GET_INT(env, argv[0], &((int)handle));
    NIF_GET_INT(env, argv[1], &priority);
    priority += 1;
    NIF_ERR( alloc_string_from_term(env, argv[2], &message_text) );

    // message ID 0x1000 is backed by a string resource containing the
    // replacement pattern '%1' which is substituted with the string argument
    // to this function.
    WINAPI_LASTERR(env, ReportEvent(handle,
                                     get_type(priority),
                                     (WORD)priority,
                                     0x1000,
                                     NULL,
                                     1,
                                     0,
                                     &message_text,
                                     NULL));
    LocalFree(message_text);

    return atom_ok;
}

ERL_NIF_TERM
deregister_event_source(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    HANDLE handle;

    NIF_GET_INT(env, argv[0], &((int)handle));

    WINAPI_LASTERR(env, DeregisterEventSource(handle));

    return atom_ok;
}

