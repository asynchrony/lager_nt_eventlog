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

  Refer to Lager's documentation for futher information on configuring handlers.

# THANKS

  This code would not have been possible without the prior art of the following
  open-source projects:

  * [Lager Syslog backend](https://github.com/basho/lager_syslog)
  * [Apache log4j](http://logging.apache.org/log4j)
