Dim Shared blocks(2) As UInteger 'Block data for 5577 card

Dim As Any Ptr library = DyLibLoad("SRF32.dll") '' load dll

'SRF32.dll functions
Dim Shared s_init As Function (ByVal port As Integer, ByVal baud As Long, ByVal dtr As Integer, ByVal rts As Integer) As Long
Dim Shared s_exit As Function (ByVal h_dev As Long) As Integer
Dim Shared s_bell As Function (ByVal h_dev As Long, ByVal Time As Integer) As Integer
Dim Shared s_t5577_blockRead As Function (ByVal h_dev As Long, ByVal rate As Integer, ByVal page As Integer, ByVal block As Integer, buffer As UInteger Ptr) As Integer
Dim Shared s_t5577_blockWrite As Function (ByVal h_dev As Long, ByVal rate As Integer, ByVal page As Integer, ByVal block As Integer, buffer As UInteger Ptr) As Integer
Dim Shared s_t5577_reset As Function (ByVal h_dev As Long, ByVal rate As Integer, buffer As UInteger Ptr) As Integer
Dim Shared s_t5577_reset_ascii As Function (ByVal h_dev As Long, ByVal rate As Integer, ByVal sdata As String) As Integer


s_init = DyLibSymbol(library, "s_init")
s_exit = DyLibSymbol(library, "s_exit")
s_bell = DyLibSymbol(library, "s_bell")
s_t5577_blockRead = DyLibSymbol(library, "s_t5577_blockRead")
s_t5577_blockWrite = DyLibSymbol(library, "s_t5577_blockWrite")
s_t5577_reset = DyLibSymbol(library, "s_t5577_reset")
s_t5577_reset_ascii = DyLibSymbol(library, "s_t5577_reset_ascii")

'------------------------------------------------
Declare Sub ExitError(msg As String)
Declare Function Round(dividend As Integer, divisor As Integer) As Integer
Declare Function Bin_to_int(bits As String) As Double
Declare Sub Create_blocks ( start As Integer, text As String )
Declare Function Decode_bitstream( PreamblePos As Integer, text As String ) As Integer
Declare Sub Simple_decoding()
Declare Sub Average_decoding()
Declare Sub Open_wave()
Declare Sub Print_menu()
Declare Sub Write_T5577()
'------------------------------------------------

Dim key As String

Print_Menu()

Do
	key = Inkey
	If key = "1" Then
		Print 
		'Open Wave file
		Open_wave()
		'Find preamble and decode bitstream
		Simple_decoding()
		'-----
		Print
		Print "Press any key to continue"
		Sleep
		Cls
		Print_Menu()
	EndIf
	If key = "2" Then
		Print 
		'Write data to 5577 card
		Write_T5577()
		'-----
		Print
		Print "Press any key to continue"
		Sleep
		Cls
		Print_Menu()
	EndIf
Loop Until key="3"

End





'------------------------------------------------
' Subroutines and functions
'------------------------------------------------

Sub ExitError(msg As String)
  Print msg
  Sleep
  End
End Sub

Function Round(dividend As Integer, divisor As Integer) As Integer
	Dim Res As Integer
	Res = (dividend + (divisor \ 2)) \ divisor
  Return Res
End Function

Function Bin_to_int(bits As String) As Double
    Dim num As Double
    Dim i As Integer
    
    For i = 1 To Len(bits)
        If (Mid(bits, i, 1)="1") Then
            num = num + 2 ^ (Len(bits) - i)
        EndIf
    Next i
    
    Bin_to_int = num    
End Function

Sub Create_blocks ( start As Integer, text As String )
	Dim data_byte(10) As UByte
	Dim c As UByte
	Dim i As Integer
	
	c = 0
	For i=start To start+88-2 Step 8 'Start forming all the bytes for the 5577 blocks
		data_byte(c) = 0
		If (Mid(text,i,1)="1") Then
			data_byte(c) = data_byte(c) + 128
		EndIf
		If (Mid(text,i+1,1)="1") Then
			data_byte(c) = data_byte(c) + 64
		EndIf
		If (Mid(text,i+2,1)="1") Then
			data_byte(c) = data_byte(c) + 32
		EndIf
		If (Mid(text,i+3,1)="1") Then
			data_byte(c) = data_byte(c) + 16
		EndIf
		If (Mid(text,i+4,1)="1") Then
			data_byte(c) = data_byte(c) + 8
		EndIf
		If (Mid(text,i+5,1)="1") Then
			data_byte(c) = data_byte(c) + 4
		EndIf
		If (Mid(text,i+6,1)="1") Then
			data_byte(c) = data_byte(c) + 2
		EndIf
		If (Mid(text,i+7,1)="1") Then
			data_byte(c) = data_byte(c) + 1
		EndIf
		c = c + 1
	Next

	blocks(0) = CInt("&H1D") + (data_byte(0)*256) + (data_byte(1)*65536) + (data_byte(2)*16777216)
	blocks(1) = data_byte(3) + (data_byte(4)*256) + (data_byte(5)*65536) + (data_byte(6)*16777216)
	blocks(2) = data_byte(7) + (data_byte(8)*256) + (data_byte(9)*65536) + (data_byte(10)*16777216)
		
End Sub

Function Decode_bitstream( PreamblePos As Integer, text As String ) As Integer
	
	Dim i As Integer
	Dim decode As String
	Dim start As Integer
	Dim CompanyOEM As String
	Dim CardFormat As String
	Dim SiteCode As String
	Dim CardNumber As String

	'Start decoding
	decode = ""
	Print "Start Manchester Decoding..."
	start = PreamblePos+8
	For i=start To start+88-2 Step 2 '44 bits x 2 (manchester encoding)
		If (Mid(text,i,2)="01") Then
			decode = decode & "0"
		ElseIf (Mid(text,i,2)="10") Then
			decode = decode & "1"
		ElseIf (Mid(text,i,2)="11") Or (Mid(text,i,2)="00") then
			Print "Error during Manchester decoding! I try to find another preamble..."
			Decode_bitstream = start + 1
		EndIf
	Next
	
	Print "Manchester Decoding Ok!"
	
	'Create the blocks for the 5577 card
	Create_blocks(start,text)
		
	'Extracting fields
	CompanyOEM = Mid(decode,1,7)
	CardFormat = Mid(decode,8,11)
	SiteCode   = Mid(decode,20,8)
	CardNumber = Mid(decode,28,Len(decode)-28)
	
	'------------------------------------------------
	'	Parity checking
	'------------------------------------------------ 
	
	'Check Parity (Even parity for sitecode)
	Dim par As Integer
	
	par = 0
	For i=1 To Len(SiteCode)
		If (Mid(SiteCode,i,1)="1") Then
			par = par + 1
		EndIf
	Next
	If (par Mod 2)=0 Then
		If Mid(decode,19,1)<>"0" Then
			Print "Wrong parity found! I try to find another preamble..."
			Decode_bitstream = start + 1
		EndIf
	Else
		If Mid(decode,19,1)<>"1" Then
			Print "Wrong parity found! I try to find another preamble..."
			Decode_bitstream = start + 1
		EndIf
	EndIf
	
	'Check Parity (Odd parity for a part of cardnumber)
	par = 0
	For i=5 To Len(CardNumber)
		If (Mid(CardNumber,i,1)="1") Then
			par = par + 1
		EndIf
	Next
	If (par Mod 2)=0 Then
		If Mid(decode,Len(decode),1)<>"1" Then
			Print "Wrong parity found! I try to find another preamble..."
			Decode_bitstream = start + 1
		EndIf
	Else
		If Mid(decode,Len(decode),1)<>"0" Then
			Print "Wrong parity found! I try to find another preamble..."
			Decode_bitstream = start + 1
		EndIf
	EndIf
	
	'------------------------------------------------
	'	Displaying informations
	'------------------------------------------------ 
	
	Print "CompanyOEM =  ";CompanyOEM;" (";Bin_to_int(CompanyOEM);")dec"
	Print "CardFormat = ",CardFormat;" (";Bin_to_int(CardFormat);")dec"
	Print "SiteCode   = ",SiteCode  ;" (";Bin_to_int(SiteCode  );")dec"
	Print "CardNumber = ",CardNumber;" (";Bin_to_int(CardNumber);")dec"
	
	Decode_bitstream = 0

End Function 

Sub Simple_decoding()
	
	Dim i As Integer
	Dim As Integer bitFile
	Dim start As Integer
	Dim text As String
	
	bitFile = FreeFile()
	Open "bitstream.txt" For Input As #bitFile
	Input #bitFile, text 

	start = 1
	Do	
		
		'Find preamble byte 0x1D (00011101)
		For i=start To Len(text)-8-88
			If (Mid(text,i,8)="00011101") Then
				Print "Preamble Found!"
				Exit For
			EndIf
		Next	
		start = Decode_bitstream(i,text)
		
	Loop Until (start=0 Or start>=(Len(text)-8-88)) 'Looping until i found a preamble or i reach end of text
	
	Close #bitfile
	
	If (start=0) Then
		Print "Decoding Complete"
	Else
		Print "I cannot find a valid preamble. Try another wav file"
	End If
	
End Sub

Sub Average_decoding()
	
	Dim i As Integer
	Dim j As Integer
	Dim n_0 As Integer
	Dim n_1 As Integer
	Dim count As Integer
	Dim As Integer bitFile
	Dim num_pre As Integer
	Dim text As String
	Dim decode As String
	
	bitFile = FreeFile()
	Open "bitstream.txt" For Input As #bitFile
	Input #bitFile, text 

	'Finding all the preambles in the wave file
	Print "Finding all the preambles in the file..."
	num_pre = 0
	For i=1 To Len(text)-8-88
		If (Mid(text,i,8)="00011101") Then
			num_pre = num_pre + 1
		EndIf
	Next	
	
	'Create an array with all the bitstream data found
	Dim Bitfound(num_pre) As String
	
	count = 1
	For i=1 To Len(text)-8-88
		If (Mid(text,i,8)="00011101") Then
			Bitfound(count) = Mid(text,i,8+88)
			count = count + 1
		EndIf
	Next	
	
	Print "I found";count-1;" preambles"
	
	'Start averaging
	decode = ""
	n_0 = 0
	n_1 = 0
	For i=1 To Len(Bitfound(1))
		For j=1 To count-1
			If Mid(Bitfound(j),i,1)="1" Then
				n_1	= n_1 + 1
			ElseIf Mid(Bitfound(j),i,1)="0" Then
				n_0	= n_0 + 1
			EndIf
		Next
		If n_1 > n_0 Then
			decode = decode & "1"
		Else
			decode = decode & "0"
		EndIf
		n_0 = 0
		n_1 = 0
	Next
	
	'Start decoding
	Decode_bitstream(1,decode)
	
	Close #bitfile
	
	
End Sub

Sub Open_wave()
	Dim As Integer myHandle
	Dim wavInput As String

	myHandle = FreeFile()
	Input "Insert the wav file name (without .wav): ", wavInput
	Open wavInput & ".wav" For Binary Access Read As #myHandle
	Print

	Dim buffer As UByte
	Dim i As Integer
	Dim ChunkID As String
	Dim ChunkSize As UInteger
	Dim Format As String
	Dim Subchunk1ID As String
	Dim Subchunk1Size As UInteger
	Dim AudioFormat As UShort
	Dim NumChannels As UShort 
	Dim	SampleRate As UInteger
	Dim ByteRate As UInteger  
	Dim BlockAlign As UShort
	Dim BitsPerSample As UShort
	Dim Subchunk2ID As String
	Dim Subchunk2Size As UInteger
	Dim Sample As Short
	
	'------------------------------------------------
	'	Read ChunkID	
	'------------------------------------------------  
	For i As Integer = 0 To 3
	  Get #myHandle, , buffer
	  ChunkID = ChunkID & Chr(buffer)
	Next i
	
	If (ChunkId<>"RIFF") Then	
	  ExitError "Error while opening wav file"  
	End If
	'------------------------------------------------  
	
	'------------------------------------------------
	'	Read ChunkSize	
	'------------------------------------------------  
	Get #myHandle, , ChunkSize
	'------------------------------------------------  	
	
	'------------------------------------------------
	'	Read Format	
	'------------------------------------------------  
	For i As Integer = 0 To 3
	  Get #myHandle, , buffer
	  Format = Format & Chr(buffer)
	Next i
	
	If (Format<>"WAVE") Then	
	  ExitError "Error while opening wav file"  
	End If
	'------------------------------------------------  
	
	'------------------------------------------------
	'	Read Subchunk1ID	
	'------------------------------------------------  
	For i As Integer = 0 To 3
	  Get #myHandle, , buffer
	  Subchunk1ID = Subchunk1ID & Chr(buffer)
	Next i
	
	If (Subchunk1ID<>"fmt ") Then	
	  ExitError "Error while opening wav file"  
	End If
	'------------------------------------------------ 
	
	'------------------------------------------------
	'	Read Subchunk1Size	
	'------------------------------------------------  
	Get #myHandle, , Subchunk1Size
	'------------------------------------------------ 
	
	'------------------------------------------------
	'	Read AudioFormat
	'------------------------------------------------  
	Get #myHandle, , AudioFormat
	'------------------------------------------------ 
	
	'------------------------------------------------
	'	Read NumChannels 
	'------------------------------------------------  
	Get #myHandle, , NumChannels 
	'------------------------------------------------  
	
	'------------------------------------------------
	'	Read SampleRate 
	'------------------------------------------------  
	Get #myHandle, , SampleRate 
	'------------------------------------------------ 
	
	'------------------------------------------------
	'	Read ByteRate 
	'------------------------------------------------  
	Get #myHandle, , ByteRate 
	'------------------------------------------------  
	 
	'------------------------------------------------
	'	Read BlockAlign 
	'------------------------------------------------  
	Get #myHandle, , BlockAlign 
	'------------------------------------------------   
	  
	'------------------------------------------------
	'	Read BitsPerSample 
	'------------------------------------------------  
	Get #myHandle, , BitsPerSample 
	'------------------------------------------------  
	
	If (ByteRate<>(SampleRate * NumChannels * BitsPerSample/8)) Then
		ExitError "Error while opening wav file" 		
	End If
	
	If (BlockAlign<>(NumChannels * BitsPerSample/8)) Then
		ExitError "Error while opening wav file" 		
	End If
	
	'------------------------------------------------
	'	Read Subchunk2ID	
	'------------------------------------------------  
	For i As Integer = 0 To 3
	  Get #myHandle, , buffer
	  Subchunk2ID = Subchunk2ID & Chr(buffer)
	Next i
	
	If (Subchunk2ID<>"data") Then	
	  ExitError "Error while opening wav file"  
	End If
	'------------------------------------------------ 
	  
	'------------------------------------------------
	'	Read Subchunk2Size 
	'------------------------------------------------  
	Get #myHandle, , Subchunk2Size 
	'------------------------------------------------ 
	
	If (ChunkSize<>(4 + (8 + SubChunk1Size) + (8 + SubChunk2Size))) Then
		ExitError "Error while opening wav file" 		
	End If   
	
	'------------------------------------------------
	'	Read Data
	'------------------------------------------------ 
	
	Dim As Integer bitFile
	bitFile = FreeFile()
	Open "bitstream.txt" For Output As #bitFile
	
	Print "Extracting bitstream from wave file..."
	
	Dim length As Integer
	Dim cycle As Byte
	Dim flag As Byte
	Dim cbit As Integer
	
	length = 0
	cycle = 0
	flag = 0
	cbit = 0
	
	Do Until EOF(myHandle)
	  Get #myHandle, , Sample
	  If (Sample>0) Then
	  	If (cycle=0) Then
	  		If (length>14) Then
	  			If (flag=0) Then
	  				For i = 1 To Round(cbit,6)
	  					Print #bitFile, "0";
	  				Next
	  				cbit = 0
	  			EndIf
	  			flag = 1
	  		Else
	  			If (flag=1) Then
	  				For i = 1 To Round(cbit,5)
	  					Print #bitFile, "1";
	  				Next
	  				cbit = 0
	  			EndIf
	  			flag = 0
	  		EndIf
	  		cbit = cbit + 1
	  		length = 0
	  	EndIf
	  	cycle = 1
	  Else
	  	cycle = 0	
	  EndIf
	  
	  length = length + 1
	Loop
	
	Print "Done!"
	
	Close #bitfile
	Close #myHandle
	
End Sub

Sub Print_menu()
	Print "HID 26 bit cloner - By Ptr"
	Print
	Print "1 - Open wav file"
	Print "2 - Write HID data into a T5577 card"
	Print "3 - Exit"
End Sub

Sub Write_T5577()
	Dim h_dev As Integer
	Dim blockBytes(14) As UInteger
	Dim block As UInteger
	Dim i As Integer
	Dim st As Integer
	Dim databuff32 As ZString*32
	
	Print "I write these blocks into the card "	
	Print "Block 0 : 60701000 "	
	Print "Block 1 : ";Hex(blocks(0),8)	
	Print "Block 2 : ";Hex(blocks(1),8)	
	Print "Block 3 : ";Hex(blocks(2),8)	
	Print
	'Device initialization
	h_dev = s_init(0, 0, 0, 0)
	If h_dev = 0 Then
		Print "ERROR: Device not connected"
		Exit Sub
	EndIf
	
	Print "I will use manchester modulation to write and read back blocks 1 - 2 - 3"
	
	'Write block 0 for manchester encoding data
	block = (CUInt("&H" & "E8800800"))
	st = s_t5577_blockWrite(h_dev, 0, 0, 0, @block)
	
	st = s_t5577_reset(h_dev, 0, @block)
	If st<>28 Then
		Print "ERROR: Card not present"
		Exit Sub
	EndIf
	
	'Write data
	st = s_t5577_blockWrite(h_dev, 0, 0, 1, @blocks(0))
	st = s_t5577_blockWrite(h_dev, 0, 0, 2, @blocks(1))
	st = s_t5577_blockWrite(h_dev, 0, 0, 3, @blocks(2))
	
	'Read back data
	For i=1 To 3
		st = s_t5577_blockRead(h_dev, 0, 0, i, @blockBytes(0))	
		If (st<>4) Then
			Print "ERROR: Read fail for block ";i
			Exit Sub
		EndIf
		If (blockBytes(0)<>blocks(i-1)) Then
			Print "ERROR: Write fail for block ";i
			Exit Sub
		EndIf
	Next
	
	Print "Readback Ok!"
	Print "Write block 0 for FSK encoding..."
	
	'Write block 0 for FSK encoding
	block = (CUInt("&H" & "60701000"))
	st = s_t5577_blockWrite(h_dev, 0, 0, 0, @block)
	
	'Ring the bell
	st = s_bell(h_dev, 1)
	Print "Card Cloned!"
	
End Sub

