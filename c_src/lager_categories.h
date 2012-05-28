//
//  Values are 32 bit values laid out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//


//
// Define the severity codes
//


//
// MessageId: LAGER_LOG_NONE
//
// MessageText:
//
// None
//
#define LAGER_LOG_NONE                   0x00000000L

//
// MessageId: LAGER_EMERGENCY
//
// MessageText:
//
// Emergency
//
#define LAGER_EMERGENCY                  0x00000001L

//
// MessageId: LAGER_ALERT
//
// MessageText:
//
// Alert
//
#define LAGER_ALERT                      0x00000002L

//
// MessageId: LAGER_CRITICAL
//
// MessageText:
//
// Critical
//
#define LAGER_CRITICAL                   0x00000003L

//
// MessageId: LAGER_ERROR
//
// MessageText:
//
// Error
//
#define LAGER_ERROR                      0x00000004L

//
// MessageId: LAGER_WARNING
//
// MessageText:
//
// Warning
//
#define LAGER_WARNING                    0x00000005L

//
// MessageId: LAGER_NOTICE
//
// MessageText:
//
// Notice
//
#define LAGER_NOTICE                     0x00000006L

//
// MessageId: LAGER_INFO
//
// MessageText:
//
// Info
//
#define LAGER_INFO                       0x00000007L

//
// MessageId: LAGER_DEBUG
//
// MessageText:
//
// Debug
//
#define LAGER_DEBUG                      0x00000008L

//
// MessageId: 0x00001000L (No symbolic name defined)
//
// MessageText:
//
// %1
//


