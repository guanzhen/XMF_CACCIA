'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------


'--- xml-files ------------------------------------
#define DecoderXml      "config\Decoder.xml"
#define AckTable        "\config\AckTable.xml"
#define CmdTable        "\config\CommandTable.xml"
#define ACK_WORK_FILE   "\work\AckWork.xml"

'--- constants ------------------------------------
#define ComInfoColor        "#505050"
#define SimInfoColor        "#FF7C00"
#define DwnldColor          "#00008B"
#define MessageColor        "#006400"

#define CMD_ACK                     0
#define DOWNLOAD_ACK                1
#define MEMORY_ACK                  2

'General ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define ACK_OK                   0x00
#define ACK_NOK                  0x01
#define ACK_UNKOWN_CMD           0x02


'Device params ~~~~~~~~~~~~~~~~~~~~~~
#define comDevice              "COM1"
#define comBaudRate            115200
#define comParity               False
#define comStopBits                 1
#define comByteSize                 8

'Device Id default ~~~~~~~~~~~~~~~~~~
#define DevIdProdDate    "18.11.2016"
#define DevIdManufact           "BMK"
#define DevIdSerNo           "000001"
#define DevIdMatNo          "1234567"
#define DevIdNewPart              "-"
#define DevIdFuncState           "01"
#define DevIdRevState            "01"


#define MAX_ROWS                 100

' decoder defines ~~~~~~~~~~~~~~~~~~
#define HEX2                       0
#define HEX4                       1
#define DEZ                        2

' Serial scope defines ~~~~~~~~~~~~~
#define MAX_LINE_SERIAL_SCOPE    100

' Commands ~~~~~~~~~~~~~~~~~~~~~~~~~
#define DOWNLOAD_SOF            0x01
#define DOWNLOAD_LINE           0x02
#define DOWNLOAD_EOF            0x04
#define TARGET_APP              0x30
#define CMD_SEND_DATA           0x09
#define CMD_GET_DATA            0x6a
#define PARAM_GET_MEMORY_START  0x1B
#define PARAM_GET_MEMORY_LINE   0x1D
#define PARAM_GET_MEMORY_END    0x1E
#define PARAM_MEMORY_SINGLE     0x1F

'Ackerrors ~~~~~~~~~~~~~~~~~~~~~~~~~
#define ACK_INVALID_PARAM       0x05
#define ACK_NO_MORE_DATA        0x10



