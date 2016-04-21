# Overview
  This is an NT Event Log backend for [Lager](https://github.com/basho/lager).
  It allows you to send messages out of lager and into the local NT Event Log.

# Configuration
  Configure a Lager handler like the following:

```erlang
{lager_nt_eventlog_backend, [Source, Level]}
```

  `Source` is the string to tag all messages with in the Event Log, usually the
  application's name.

  `Level` is the lager level at which the backend accepts messages (eg. using
  `info` would send all messages at info level or above into the Event Log).

  An example might look something like this:

```erlang
{lager_nt_eventlog_backend, ["product", info]}
```

## Custom Formatting
  A custom formatter can be supplied via the configuration:
  
```erlang
{lager_nt_eventlog_backend, [Source, Level, {Formatter, FormatConfig}]}
```
  
  `Formatter` is an atom of a module the implements `format/2` and `format/3` callbacks.
  if a formatter is not specified, the `lager_default_formatter` will be used by default.
  
  `FormatConfig` is an iolist that the `Formatter` uses to construct the message.
  if a formatter is not specified, the following is used to construct log messages similar to Lager 1.0
  
```erlang
[
  {pid, ""},
  {module, [
    {pid, ["@"], ""},
    module,
    {function, [":", function], ""},
    {line, [":",line], ""}], ""},
  " ",
  message
]
```

  Refer to Lager's documentation for futher information on configuring handlers.

# THANKS

  This code would not have been possible without the prior art of the following
  open-source projects:

  * [Lager Syslog backend](https://github.com/basho/lager_syslog)
  * [Apache log4j](http://logging.apache.org/log4j)
