#ifndef REGISTRY_HELPERS_H
#define REGISTRY_HELPERS_H

#include <windows.h>

LONG
open_registry_key(HKEY hive, LPCTSTR subkey, PHKEY key, LPDWORD disposition);

LONG
set_registry_string_value(HKEY key, LPCTSTR value_name, LPCTSTR value);

LONG
set_registry_dword_value(HKEY key, LPCTSTR value_name, CONST DWORD *value);

#endif /* ndef REGISTRY_HELPERS_H */
