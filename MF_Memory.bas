'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------



'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~ MEMORY DUMP ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnClick_BUTTON_READ_MEMORY(Reason)
    
  Dim AdrOffs, Adr, StartAdr, EndAdr
  Dim Source, DataPtr
  Dim Found, EndRead, ReadLoop
  Dim MaxWords, Field, i, tmp
  Dim Cmd, maxBytesDisp, perCent, perCentOld
  Dim dispType
  Dim sendData(), dataLen, DoIt, readData(), noMoreData

  Window.BeginWaitingCursor 9
  HighlightButton "BUTTON_READ_MEMORY", True
  DisableButtons

  If Visual.Select("MemDataFormat").SelectedIndex = 0 Then
    dispType = " bytes " 
  Else
    dispType = " words "
  End If
  Visual.Select("HexEditCtrl").Object.SetData Field, 0
  MaxWords = Visual.Select("HexEditCtrl").Object.Size
  Field = Object.SafeArrayCreate( MaxWords )
  '--- Dummy data -------------------------------------------------------------
  For i = 0 To MaxWords - 1
    Field( i ) = 0
  Next
  '----------------------------------------------------------------------------
  DoIt = $(ACK_NOK)
  If checkAdresses (True, StartAdr, EndAdr) = $(ACK_OK) Then
    ReDim sendData(16)
    dataLen     = 9
    senddata(0) = $(CMD_GET_DATA)
    sendData(1) = 7
    sendData(2) = $(PARAM_GET_MEMORY_START)
    sendData(3) = 0
    sendData(4) = String.SafeParse(Visual.SelMemTarget.SelectedItemAttribute("value"))
    sendData(5) = Lang.LoByte(Lang.LoWord(CLng(StartAdr)))
    sendData(6) = Lang.HiByte(Lang.LoWord(CLng(StartAdr)))
    sendData(7) = Lang.LoByte(Lang.HiWord(CLng(StartAdr)))
    sendData(8) = Lang.HiByte(Lang.HiWord(CLng(StartAdr)))
    sendComMessage sendData , dataLen
    DoIt = wait4Ack ( $(MEMORY_ACK), 4, readData) 
  End If
  If DoIt = $(ACK_OK) Then
    ' Send Lines -----
    ReDim sendData(8)
    dataLen     = 3
    senddata(0) = $(CMD_GET_DATA)
    sendData(1) = 1
    sendData(2) = $(PARAM_GET_MEMORY_LINE)
    EndRead    = False
    noMoreData = False
    AdrOffs    = 0
    perCentOld = 0
    
    maxBytesDisp = 512 * (Visual.Select("MemDataFormat").SelectedIndex + 1)
    Do While AdrOffs < MaxWords and EndRead = False 
      sendComMessage sendData , dataLen
      DoIt = wait4Ack ( $(MEMORY_ACK), 20, readData) 
      If Not DoIt = $(ACK_OK) Then
        Exit Do
      Else
        If readData(2) = $(ACK_NO_MORE_DATA) Then 
          EndRead    = True
          noMoreData = True
        Else 
          If Not readData(2) = 0 Then 
            System.MessageBox  "ACK-ERROR: " & getAckErrStr( readData(2) ), "Acknowledge error" , MB_ICONEXCLAMATION
            Exit Do 
          Else
            If readData(1) <= 3 Then
              System.MessageBox  "DATA-ERROR: wrong data length", "Acknowledge error" , MB_ICONEXCLAMATION
              Exit Do 
            End If
          End If 
        End If 
        DataPtr = 3
        Do While DataPtr < readData(1)+2
          Field(AdrOffs) = readData(DataPtr)
          DataPtr = DataPtr + 1
          AdrOffs = AdrOffs + 1
          If AdrOffs >= MaxWords Then
            EndRead = True
            Exit Do 
          End If 
        Loop
        Window.LockWindowUpdate true
        percent = CInt(AdrOffs / (MaxWords / 100.0))
        If Not percent = percentOld Then
          Visual.ProgressBar.Object.setPercentage percent
          percentOld = percent
        End If
        If AdrOffs Mod 32 = 0 And adrOffs < maxBytesDisp Then
          Visual.Select("HexEditCtrl").Object.SetData Field, StartAdr
        End If
        Window.LockWindowUpdate false
      End If 
    Loop

    If DoIt = $(ACK_OK) Then
      Window.LockWindowUpdate true
      Visual.Select("HexEditCtrl").Object.SetData Field, StartAdr
      Window.LockWindowUpdate false
      Visual.ProgressBar.Object.setPercentage 100
      If EndRead = True And Not AdrOffs = MaxWords And noMoreData = False Then 
        Visual.Select("MemoryStartAdr").Style.BackGroundColor = "Red"
        Visual.Select("MemoryEndAdr").Style.BackGroundColor   = "Red"
        If Visual.MemDataFormat.SelectedIndex = 0 Then
          MsgBox "Wrong data lenght! --  Received = " & AdrOffs & dispType & "/ Expected = " & MaxWords & dispType, vbCritical, "Read from memory"
        Else
          MsgBox "Wrong data lenght! --  Received = " & AdrOffs/2 & dispType & "/ Expected = " & MaxWords/2 & dispType, vbCritical, "Read from memory"
        End If
        Visual.Select("MemoryStartAdr").Style.BackGroundColor = ""
        Visual.Select("MemoryEndAdr").Style.BackGroundColor   = ""
      Else
        sendData(2) = $(PARAM_GET_MEMORY_END)
        sendComMessage sendData , dataLen
        DoIt = wait4Ack ( $(MEMORY_ACK), 4, readData) 
      End If 
      If noMoreData = False Then
        If Visual.Select("MemDataFormat").SelectedIndex = 0 Then
          MsgBox AdrOffs & dispType & "received", vbInformation, "Read from memory"
        Else
          MsgBox AdrOffs/2 & dispType & "received", vbInformation, "Read from memory"
        End If
      Else
        If Visual.Select("MemDataFormat").SelectedIndex = 0 Then
          MsgBox "No more data / " & AdrOffs & dispType & "received", vbInformation, "Read from memory"
        Else
          MsgBox "No more data / " & AdrOffs/2 & dispType & "received", vbInformation, "Read from memory"
        End If
      End If
    End If
  End If
  Visual.ProgressBar.Object.setPercentage 0
  HighlightButton "BUTTON_READ_MEMORY", False
  EnableButtons
  Window.EndWaiting
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~ MEMORY SINGLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnClick_BUTTON_READ_WORD(Reason)
  Dim sendData(), readData()
  Dim StartAdr
  Dim DoIt

  DoIt = $(ACK_OK)
  HighlightButton "BUTTON_READ_WORD", True
  DisableButtons
  If Not String.Parse (Visual.Select("WordMemoryAdr").Value, StartAdr) = True Then 
    Visual.Select("WordMemoryAdr").Style.BackGroundColor = "Red"
    System.MessageBox  "! Please use hex format for start address !", "Format error" , MB_ICONEXCLAMATION
    DoIt = $(ACK_NOK)
  Else
    Visual.Select("WordMemoryAdr").Style.BackGroundColor = ""
  End If
  If DoIt = $(ACK_OK) Then
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ReDim sendData(16)
    senddata(0) = $(CMD_GET_DATA)
    sendData(1) = 7
    sendData(2) = $(PARAM_MEMORY_SINGLE)
    sendData(3) = 0
    sendData(4) = String.SafeParse(Visual.SelMemTarget.SelectedItemAttribute("value"))
    sendData(5) = Lang.LoByte(Lang.LoWord(CLng(StartAdr)))
    sendData(6) = Lang.HiByte(Lang.LoWord(CLng(StartAdr)))
    sendData(7) = Lang.LoByte(Lang.HiWord(CLng(StartAdr)))
    sendData(8) = Lang.HiByte(Lang.HiWord(CLng(StartAdr)))
    sendComMessage sendData , 9
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    If wait4Ack ( $(MEMORY_ACK), 8, readData) = $(ACK_OK) Then
      If Visual.SelReadMemSingleVal.SelectedIndex = 0 Then  
        ' Display byte values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Visual.WordReadVal.Value = String.Format("0x%02X 0x%02X 0x%02X 0x%02X", readData(3),readData(4),readData(5),readData(6))
      ElseIf Visual.SelReadMemSingleVal.SelectedIndex = 1 Then
        ' Display word values ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Visual.WordReadVal.Value = String.Format("0x%04X 0x%04X", Lang.MakeWord(readData(3),readData(4)),Lang.MakeWord(readData(5),readData(6)))
      Else
        ' Display long value ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Visual.WordReadVal.Value = String.Format("0x%08X", Lang.MakeLong4(readData(3),readData(4),readData(5),readData(6)))
      End If
    End If
  End If
  HighlightButton "BUTTON_READ_WORD", False
  EnableButtons
  Window.EndWaiting
End Sub


Sub OnClick_BUTTON_WRITE_WORD(Reason)
  Dim sendData(), readData()
  Dim StartAdr, writeValue
  Dim DoIt

  DoIt = $(ACK_OK)
  HighlightButton "BUTTON_WRITE_WORD", True
  DisableButtons
  If Not String.Parse (Visual.Select("WordMemoryAdr").Value, StartAdr) = True Then 
    Visual.Select("WordMemoryAdr").Style.BackGroundColor = "Red"
    System.MessageBox  "! Please use hex format for start address !", "Format error" , MB_ICONEXCLAMATION
    DoIt = $(ACK_NOK)
  Else
    Visual.Select("WordMemoryAdr").Style.BackGroundColor = ""
  End If
  If Not String.Parse (Visual.Select("WordWriteVal").Value, writeValue) = True Then 
    Visual.Select("WordWriteVal").Style.BackGroundColor = "Red"
    System.MessageBox  "! Please use correct format for write value !", "Format error" , MB_ICONEXCLAMATION
    DoIt = $(ACK_NOK)
  Else
    Visual.Select("WordWriteVal").Style.BackGroundColor = ""
  End If
  If DoIt = $(ACK_OK) Then
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ReDim sendData(16)
    senddata(0)  = $(CMD_SEND_DATA)
    sendData(1)  = 9
    sendData(2)  = $(PARAM_MEMORY_SINGLE)
    sendData(3)  = 0
    sendData(4)  = String.SafeParse(Visual.SelMemTarget.SelectedItemAttribute("value"))
    sendData(5)  = Lang.LoByte(Lang.LoWord(CLng(StartAdr)))
    sendData(6)  = Lang.HiByte(Lang.LoWord(CLng(StartAdr)))
    sendData(7)  = Lang.LoByte(Lang.HiWord(CLng(StartAdr)))
    sendData(8)  = Lang.HiByte(Lang.HiWord(CLng(StartAdr)))
    sendData(9)  = Lang.LoByte(writeValue)
    sendData(10) = Lang.HiByte(writeValue)
    sendComMessage sendData , 11
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    wait4Ack  $(MEMORY_ACK), 4, readData
  End If
  HighlightButton "BUTTON_WRITE_WORD", False
  EnableButtons
  Window.EndWaiting
End Sub

'---------------------------------------------------------------------------------------------------
'--- Select memory size ----------------------------------------------------------------------------
'---------------------------------------------------------------------------------------------------
Sub SetHexEditCtrl( startAdr, endAdr )
  Dim MaxWords, Field, LoopCnt

  If endAdr > startAdr Then
    MaxWords = endAdr - startAdr
  Else
    MaxWords = 16
  End If
  If Visual.Select("MemDataFormat").SelectedIndex = 1 Then
    MaxWords = MaxWords * 2
  End If
  
  Field = Object.SafeArrayCreate( MaxWords )
  '--- Dummy data -------------------------------------------------------------
  For loopCnt = 0 To MaxWords - 1
    Field( loopCnt ) = 0
  Next
  '----------------------------------------------------------------------------
  Visual.Select("HexEditCtrl").Object.SetData Field, startAdr
End Sub

Sub OnChange_MemoryStartAdr( Reason )
  Dim StartAdr,EndAdr
  checkAdresses False, StartAdr, EndAdr
End Sub


Sub OnChange_MemoryEndAdr( Reason )
  checkMemoryParams
End Sub

Sub checkMemoryParams
  Dim StartAdr,EndAdr
  checkAdresses False, StartAdr, EndAdr
End Sub

Function checkAdresses (checkLimits, ByRef startAdr, ByRef EndAdr)
  Dim RetVal
  
  RetVal = $(ACK_OK)
  If Not String.Parse (Visual.Select("MemoryStartAdr").Value, StartAdr) = True Then 
    Visual.Select("MemoryStartAdr").Style.BackGroundColor = "Red"
    System.MessageBox  "! Please use hex format for start address !", "Format error" , MB_ICONEXCLAMATION
    RetVal = $(ACK_NOK)
  Else
    Visual.Select("MemoryStartAdr").Style.BackGroundColor = ""
    StartAdr = ((StartAdr+5) \ 16) * 16
    Visual.Select("MemoryStartAdr").Value = String.Format("0x%04x",StartAdr)
  End If
  If RetVal = $(ACK_OK) Then
    If Not String.Parse (Visual.Select("MemoryEndAdr").Value, EndAdr) = True Then 
      Visual.Select("MemoryEndAdr").Style.BackGroundColor   = "Red"
      System.MessageBox  "! Please use hex format for end address !", "Format error" , MB_ICONEXCLAMATION
      RetVal = $(ACK_NOK)
    Else
      Visual.Select("MemoryEndAdr").Style.BackGroundColor   = ""
      EndAdr = ((EndAdr+5) \ 16) * 16
      Visual.Select("MemoryEndAdr").Value = String.Format("0x%04x",EndAdr)
    End If
  End If
  If RetVal = $(ACK_OK)Then
    If StartAdr < EndAdr Then
      SetHexEditCtrl StartAdr, EndAdr 
    End If
  End If
  If RetVal = $(ACK_OK) And checkLimits = True Then
    If StartAdr >= EndAdr Then
      Visual.Select("MemoryStartAdr").Style.BackGroundColor = "Red"
      Visual.Select("MemoryEndAdr").Style.BackGroundColor   = "Red"
      System.MessageBox  "! Please note, End address is less or equal Start address !", "Input error" , MB_ICONEXCLAMATION
      RetVal = $(ACK_NOK)
    Else
      Visual.Select("MemoryStartAdr").Style.BackGroundColor = ""
      Visual.Select("MemoryEndAdr").Style.BackGroundColor   = ""
      SetHexEditCtrl StartAdr, EndAdr 
    End If
  End If
  checkAdresses = RetVal
End Function
