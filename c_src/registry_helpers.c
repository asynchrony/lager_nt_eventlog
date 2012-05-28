#include "registry_helpers.h"

LONG
open_registry_key(HKEY hive, LPCTSTR subkey, PHKEY key, LPDWORD disposition)
{
    return RegCreateKeyEx(hive,
                          subkey,
                          0,
                          NULL,
                          REG_OPTION_NON_VOLATILE,
                          KEY_SET_VALUE,
                          NULL,
                          key,
                          disposition);
}

LONG
set_registry_string_value(HKEY key, LPCTSTR value_name, LPCTSTR value)
{
    return RegSetValueEx(key,
                         value_name,
                         0,
                         REG_EXPAND_SZ,
                         (LPBYTE)value,
                         strlen(value) + 1);
}

LONG
set_registry_dword_value(HKEY key, LPCTSTR value_name, CONST DWORD *value)
{
    return RegSetValueEx(key,
                         value_name,
                         0,
                         REG_DWORD,
                         (LPBYTE)value,
                         sizeof(DWORD));
}

