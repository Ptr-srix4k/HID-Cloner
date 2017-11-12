// extern "C" {
// Test the dynamic library call
// input data: inputValue input value, the value range between 0-32766
// return data: return to inputValue + 1 when doing the correct time
__int16 __stdcall s_test_dll (__ int16 inputValue);

// 2. Scan the number of readers connected to the computer and get the corresponding parameters
__int16 __stdcall s_SearchUSBDevice (void);

// 3. Get the path to the current USB device
// input data: m_hUSB device handle
// usbIndex has already obtained the index in the list of devices
/ / If the computer is only connected to a reader, the parameter can be fixed to 0
// * portPath empty pointer, the size to ensure that more than 30 character space
// return data: * portPath get the string
__int16 __stdcall s_GetUSBportPath (int usbIndex, char * portPath);
__int16 __stdcall s_GetUSBDevInfo (int usbIndex, char * DevPath, char * DevName, char * DevSN);

// 5. Initialize the connection reader
// Input data: PortNum port number PortNum = 0 for USB, PortNum = 1 ~ 99 for serial port
// combaud serial port is not exceptionally, has been retrieved in the list of devices
// When the port type of the reader is USB port, this parameter is the index of the scan
/ / If the computer is only connected to a reader, the parameter can be fixed to 0
// return data: ret> 0 The connection is successful and the return value is the handle of the current device
HANDLE __stdcall s_init (__ int16 PortNum, __int32 combaud, __int16 DTR_State, __int16 RTS_State);

// 6. Disconnect from the reader
// input data: m_hUSB need to disconnect the handle of the reader
// return data: no substantive data
__int16 __stdcall s_exit (HANDLE m_hUSB);

// test the connection of the device
// input data: m_hUSB device handle
// inputValue the value entered, the value range between 0-32766
// return data: return to inputValue + 1 when doing the correct time
__int16 __stdcall s_test_device (HANDLE m_hUSB, __ int16 inputValue);


// 8. The device performs a buzzer
// input data: m_hUSB device handle
// bell_time The time unit of the buzzer execution, the range of values ​​is 1-9
/ / Return data: the correct implementation of no substantive data, or return the error code
__int16 __stdcall s_bell (HANDLE m_hUSB, __int16 bell_time);

// 9. Read the card information supported by the device
__int16 __stdcall s_GetSupportCard (HANDLE m_hUSB, LPDWORD lpCards);

// 10. Read the model name of the device
__int16 __stdcall s_GetDeviceName (HANDLE m_hUSB, UCHAR * devName);


// 11 hexadecimal data converted to ascii character function
__int16 __stdcall s_HexToAscii (UCHAR * uca_Hex, UCHAR * uca_Ascii, __ int16 uc_Length);

// 12. ascii characters are converted to hexadecimal data functions
__int16 __stdcall s_AsciiToHex (UCHAR * uca_Ascii, UCHAR * uca_Hex, __ int16 uc_Length);

// 13 hexadecimal data to calculate the 8-bit CRC checksum function, the resulting checksum is added directly to the original data
__int16 __stdcall s_CRC8Code (UCHAR * buffer, unsigned int len);

// 14 hexadecimal data check 8-bit CRC checksum function, checksum and returns 1, otherwise returns 0
__int16 __stdcall s_CRC8DeCode (UCHAR * buffer, unsigned int len);

// 15. hexadecimal data to compute the 16-bit CRC checksum function, the resulting checksum is added directly to the original data
__int16 __stdcall s_CRC16Code (UCHAR * buffer, unsigned int len);

// 16 hexadecimal data check 16-bit CRC checksum function, checksum and returns 1, otherwise returns 0
__int16 __stdcall s_CRC16DeCode (UCHAR * buffer, unsigned int len);

//////////////////////////////////////////////// ////////////////////////
// 17. EMID card reader function
//////////////////////////////////////////////// ////////////////////////
// read the card number of the EMID card
// input data: m_hUSB device handle
// * buffer read the data pointer to the card
// return data: 5 (bytes) or 10 (characters)
// * buffer read the data pointer to the card, the basic function is 5 bytes of data, Asccii function is 10 characters of data
__int16 __stdcall s_emid_read (HANDLE m_hUSB, UCHAR * buffer);
__int16 __stdcall s_emid_read_ascii (HANDLE m_hUSB, UCHAR * buffer);
//////////////////////////////////////////////// ////////////////////////
// T5557 / T5567 / T5577 card operation function
//////////////////////////////////////////////// ////////////////////////
// 19. Reset the T55x7 card
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 32
// = 1 RF / 64
// * buffer read the data pointer to the card
// Return data: the correct implementation of the card back to read the default number of bytes sent
// * buffer read the data pointer to the card
__int16 __stdcall s_t5577_reset (HANDLE m_hUSB, int DataRate, UCHAR * buffer);
__int16 __stdcall s_t5577_reset_ascii (HANDLE m_hUSB, int DataRate, UCHAR * buffer);
// read the data sent by the current card
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 32
// = 1 RF / 64
// * buffer read the data pointer to the card
// Return data: the correct implementation of the return to read the card to send the number of bytes or the number of characters
// * buffer read the data pointer to the card
__int16 __stdcall s_t5577_read_current (HANDLE m_hUSB, int DataRate, UCHAR * buffer);
__int16 __stdcall s_t5577_read_current_ascii (HANDLE m_hUSB, int DataRate, UCHAR * buffer);
// read the page on the T55x7 card
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 32
// = 1 RF / 64
// pageAddr The address of the page to be read, the value range is 0-1
// * buffer read the data pointer to the card
// Return data: the correct implementation of the return to read the card to send the number of bytes or the number of characters
// * buffer read the data pointer to the card
__int16 __stdcall s_t5577_pageRead (HANDLE m_hUSB, int DataRate, int pageAddr, UCHAR * buffer);
__int16 __stdcall s_t5577_pageRead_ascii (HANDLE m_hUSB, int DataRate, int pageAddr, UCHAR * buffer);
// 25. The block reads the T55x7 card
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 32
// = 1 RF / 64
// pageAddr The address of the page to be read, the value range is 0-1
// blockAddr The address of the block to be read, the value range is 0-7
// * buffer read the data pointer to the card
// Return data: the correct implementation of the time to return to read the number of bytes sent by the card, read the block when the correct return to 4 bytes of the number of bytes
// * buffer read the data pointer to the card, the basic function is 4 bytes of data, Asccii function is 8 characters of data
__int16 __stdcall s_t5577_blockRead (HANDLE m_hUSB, int DataRate, int pageAddr, int blockAddr, UCHAR * buffer);
__int16 __stdcall s_t5577_blockRead_ascii (HANDLE m_hUSB, int DataRate, int pageAddr, int blockAddr, UCHAR * buffer);
// 27 for the T55x7 card with a password block read operation
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 32
// = 1 RF / 64
// pageAddr The address of the page to be read, the value range is 0-1
// blockAddr The address of the block to be read, the value range is 0-7
// * password card password data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// * buffer read the data pointer to the card
// return data: 4 (bytes) or 8 (characters)
// * buffer read the data pointer to the card, the basic function is 4 bytes of data, Asccii function is 8 characters of data
__int16 __stdcall s_t5577_blockPasswordRead (HANDLE m_hUSB, int DataRate, int pageAddr, int blockAddr, UCHAR * password, UCHAR * buffer);
__int16 __stdcall s_t5577_blockPasswordRead_ascii (HANDLE m_hUSB, int DataRate, int pageAddr, int blockAddr, UCHAR * password, UCHAR * buffer);
// 29. Write a block to the T55x7 card
// input data: m_hUSB device handle
// LockBit locks the block to be manipulated, such as a nonzero value that will lock the block data and will no longer be changed after locking
// pageAddr The address of the page to be written, the value range is 0-1
// blockAddr The address of the block to be written, the value range is 0-7
// * buffer will be written to the card data pointer, the basic function of 4 bytes of data, Asccii function of 8 characters of data
// return data: 4 (bytes) or 8 (characters)
__int16 __stdcall s_t5577_blockWrite (HANDLE m_hUSB, int LockBit, int pageAddr, int blockAddr, UCHAR * buffer);
__int16 __stdcall s_t5577_blockWrite_ascii (HANDLE m_hUSB, int LockBit, int pageAddr, int blockAddr, UCHAR * buffer);
// 31. Write a password block with the T55x7 card
// input data: m_hUSB device handle
// LockBit locks the block to be manipulated, such as a nonzero value that will lock the block data and will no longer be changed after locking
// pageAddr The address of the page to be written, the value range is 0-1
// blockAddr The address of the block to be written, the value range is 0-7
// * password card password data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// * buffer will be written to the card data pointer, the basic function of 4 bytes of data, Asccii function of 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_t5577_blockPasswordWrite (HANDLE m_hUSB, int LockBit, int pageAddr, int blockAddr, UCHAR * password, UCHAR * buffer);
__int16 __stdcall s_t5577_blockPasswordWrite_ascii (HANDLE m_hUSB, int LockBit, int pageAddr, int blockAddr, UCHAR * password, UCHAR * buffer);
// 33 AOR wakeup operation on the T55x7 card
// input data: m_hUSB device handle
// * password card password data pointer
// * buffer will be written to the card data pointer, the basic function of 4 bytes of data, Asccii function of 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_t5577_WakeUp (HANDLE m_hUSB, UCHAR * password);
__int16 __stdcall s_t5577_WakeUp_ascii (HANDLE m_hUSB, UCHAR * password);

//////////////////////////////////////////////// ////////////////////////
// EM4205 / EM4305 / EM4469 card operation function
//////////////////////////////////////////////// ////////////////////////
// 35. EM4x05 / EM4x69 card login command
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 64
// = 1 RF / 32
// * password card password data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_em4305_login (HANDLE m_hUSB, int DataRate, UCHAR * password);
__int16 __stdcall s_em4305_login_ascii (HANDLE m_hUSB, int DataRate, UCHAR * password);
// 37. EM4x05 / EM4x69 card pause command
// input data: m_hUSB device handle
// return data: 1 when correct execution
__int16 __stdcall s_em4305_disable (HANDLE m_hUSB);
// 38. EM4x05 / EM4x69 card block read command
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 64
// = 1 RF / 32
// blockAddr The address of the block to be read, the value range is 0-15
// * blockData card block data pointer
// return data: 4 (bytes) or 8 (characters)
// * blockData card block data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
__int16 __stdcall s_em4305_readWord (HANDLE m_hUSB, int DataRate, int BlockAddr, UCHAR * blockData);
__int16 __stdcall s_em4305_readWord_ascii (HANDLE m_hUSB, int DataRate, int BlockAddr, UCHAR * blockData);
// 40. EM4x05 / EM4x69 card block write command
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 64
// = 1 RF / 32
// blockAddr The address of the block to be read, the value range is 2-15
// * blockData card block data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_em4305_writeWord (HANDLE m_hUSB, int DataRate, int BlockAddr, UCHAR * blockData);
__int16 __stdcall s_em4305_writeWord_ascii (HANDLE m_hUSB, int DataRate, int BlockAddr, UCHAR * blockData);
// 42. EM4x05 / EM4x69 card write protection block command
// input data: m_hUSB device handle
// DataRate card current RF divide frequency
// = 0 RF / 64
// = 1 RF / 32
// * blockData card block data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_em4305_protect (HANDLE m_hUSB, int DataRate, UCHAR * blockData);
__int16 __stdcall s_em4305_protect_ascii (HANDLE m_hUSB, int DataRate, UCHAR * blockData);

//////////////////////////////////////////////// ////////////////////////
// MF1 TypeA card operation function
//////////////////////////////////////////////// ////////////////////////
// 44. MF1 card read and write chip base station reset
// input data: m_hUSB device handle
// return data: 1 when correct execution
__int16 __stdcall s_mf1_pcd_reset (HANDLE m_hUSB);
// 43. Turn the MF1 antenna on or off
// input data: m_hUSB device handle
// status of the antenna
// = 0 Turn off the antenna
// = 1 to turn on the antenna
// return data: 1 when correct execution
__int16 __stdcall s_mf1_antenna (HANDLE m_hUSB, BOOL status);
// 46. Read the MF1-TypeA card number and card type
// input data: m_hUSB device handle
// RequestMode search card mode
// = 0x26 Find the card that does not enter hibernation
// = 0x52 Find all cards in the zone that meet the 14443A standard
// * CardType card type data pointer
// * CardUID card card number data pointer
// return data: 1 when correct execution
// * CardType card type data pointer, the basic function is 2 bytes of data, Asccii function is 4 characters of data
// * CardUID card card number data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
__int16 __stdcall s_mf1_typea_get_uid (HANDLE m_hUSB, int RequestMode, UCHAR * CardType, UCHAR * CardUID);
__int16 __stdcall s_mf1_typea_get_uid_ascii (HANDLE m_hUSB, int RequestMode, UCHAR * CardType, UCHAR * CardUID);
// 48. MF1-TypeA card to find the card
// input data: m_hUSB device handle
// RequestMode search card mode
// = 0x26 Find the card that does not enter hibernation
// = 0x52 Find all cards in the zone that meet the 14443A standard
// * CardType card type data pointer
// return data: 1 when correct execution
// * CardType card type data pointer, the basic function is 2 bytes of data, Asccii function is 4 characters of data
__int16 __stdcall s_mf1_typea_request (HANDLE m_hUSB, int RequestMode, UCHAR * CardType);
__int16 __stdcall s_mf1_typea_request_ascii (HANDLE m_hUSB, int RequestMode, UCHAR * CardType);
// 50. MF1-TypeA card anti-collision
// input data: m_hUSB device handle
// * CardUID card card number data pointer
// return data: 1 when correct execution
// * CardUID card card number data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
__int16 __stdcall s_mf1_typea_anticoll (HANDLE m_hUSB, UCHAR * CardUID);
__int16 __stdcall s_mf1_typea_anticoll_ascii (HANDLE m_hUSB, UCHAR * CardUID);
// 52. MF1-TypeA card selection card
// input data: m_hUSB device handle
// * CardUID card card number data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_mf1_typea_select (HANDLE m_hUSB, UCHAR * CardUID);
__int16 __stdcall s_mf1_typea_select_ascii (HANDLE m_hUSB, UCHAR * CardUID);
// 54. MF1-TypeA card verification password
// input data: m_hUSB device handle
// AuthMode authentication mode
// = 0x60 Verify the card's A password
// = 0x61 Verify the card's B password
// blockAddr The address of the block to be verified, in the range 0-255
// * codeKey card password data pointer, the basic function is 6 bytes of data, Asccii function is 12 characters of data
// * CardUID card card number data pointer, the basic function is 4 bytes of data, Asccii function is 8 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_mf1_typea_host_authkey (HANDLE m_hUSB, int AuthMode, int blockAddr, UCHAR * codeKey, UCHAR * CardUID);
__int16 __stdcall s_mf1_typea_host_authkey_ascii (HANDLE m_hUSB, int AuthMode, int blockAddr, UCHAR * codeKey, UCHAR * CardUID);
// 56. MF1-TypeA card read block data
// input data: m_hUSB device handle
// blockAddr The address of the block to be read, the value range is 0-255
// * DataBuffer card's block data pointer
// return data: 1 when correct execution
// * DataBuffer card block data pointer, the basic function is 16 bytes of data, Asccii function is 32 characters of data
__int16 __stdcall s_mf1_typea_block_read (HANDLE m_hUSB, int blockAddr, UCHAR * DataBuffer);
__int16 __stdcall s_mf1_typea_block_read_ascii (HANDLE m_hUSB, int blockAddr, UCHAR * DataBuffer);
// 58. MF1-TypeA card verification password
// input data: m_hUSB device handle
// blockAddr The address of the block to be written, in the range 0-255
// * DataBuffer card block data pointer, the basic function is 16 bytes of data, Asccii function is 32 characters of data
// return data: 1 when correct execution
__int16 __stdcall s_mf1_typea_block_write (HANDLE m_hUSB, int blockAddr, UCHAR * DataBuffer);
__int16 __stdcall s_mf1_typea_block_write_ascii (HANDLE m_hUSB, int blockAddr, UCHAR * DataBuffer);
// 60. Pause the selected MF1-TypeA card
// input data: m_hUSB device handle
// return data: 1 when correct execution
__int16 __stdcall s_mf1_typea_halt (HANDLE m_hUSB);



//}