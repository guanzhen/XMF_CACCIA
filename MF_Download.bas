'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------

Sub OnClick_BUTTON_LOADHEXFILE(Reason)
  Dim HexFile, FName, FDrive, FPath, FExt
  Dim Lines, i, x, lineLen
  Dim readLine, sendLine()

'  If System.FileDialog( True, OFN_HIDEREADONLY or OFN_FILEMUSTEXIST, "HEX-Files(*.hex)", FileName ) Then
  If System.FileDialog(True, OFN_HIDEREADONLY , "HEX-Files(*.hex),BHX-Files(*.bhx)",HexFile) Then
    Visual.DwnldFile.InnerHtml = HexFile
    File.SplitPath HexFile, FDrive, FPath, FName, FExt
    If Mid(FName,11,1) = "0" Then
      Visual.DwnldTarget.Value = "BIOS"
    Else
      Visual.DwnldTarget.Value = "APPLICATION"
    End If
  End If
End Sub

Sub OnClick_BUTTON_DOWNLOAD(Reason)
  Dim FileName, Hexfile
  Dim Lines, i, x, lineLen
  Dim readLine, sendLine()
  Dim DoIt, dummyData()
  
  Visual.SimInfo.InnerHtml = "DOWNLOAD"
  Visual.SimInfo.Style.backgroundColor = $(DwnldColor)  
  disableButtons
  DoIt = $(ACK_OK)
  FileName = Visual.DwnldFile.InnerHtml
  If Not File.FileExists(FileName) = True Then
    System.MessageBox  "Hex-File file does not exist !", "File error" , MB_ICONEXCLAMATION
  Else
    Lines = CheckConfLen (FileName)
    Set Hexfile = File.Open( FileName, "rt")
    Redim sendLine(3)
    ' Start of File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    sendline(0) = $(DOWNLOAD_SOF)
    sendLine(1) = 1
    sendLine(2) = $(TARGET_APP)
    sendComMessage sendLine , 3
    DoIt = wait4Ack ( $(DOWNLOAD_ACK), 5, dummyData) 
    ' Lines ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    For i = 0 to Lines-2
      If Not DoIt = $(ACK_OK) Then
        Exit For
      End If
      readLine = Hexfile.ReadLine
      lineLen = (len(readLine) \ 2) +1
      Redim sendLine(lineLen+2)
      sendLine(0) = $(DOWNLOAD_LINE)
      sendLine(1) = lineLen
      sendLine(2) = Asc( Mid(readLine,1,1) )
      For x = 0 to lineLen-2
        sendLine(x+3) = String.SafeParse("0x" & Mid(readLine,(x*2)+2,2),0)
      Next
      sendComMessage sendLine , lineLen+2
      If i > 1000 Then
        If i mod 10 = 0 Then
          Visual.ProgressBar.Object.setPercentage (CInt(i / (Lines / 100.0))) 
        End If
      Else
        Visual.ProgressBar.Object.setPercentage (CInt(i / (Lines / 100.0))) 
      End If
      DoIt = wait4Ack ( $(DOWNLOAD_ACK), 4, dummyData) 
    Next
    ' End of File ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    If DoIt = $(ACK_OK) Then
      sendline(0) = $(DOWNLOAD_EOF)
      sendLine(1) = 0
      sendComMessage sendLine , 2
      DoIt = wait4Ack ($(DOWNLOAD_ACK), 4, dummyData)
    End If
    If Not DoIt = True Then
      Visual.ProgressBar.Object.setPercentage(100)
      System.Delay(100)
    End If
    Visual.ProgressBar.Object.setPercentage(0)
    Set Hexfile = Nothing
  End If
  Visual.SimInfo.InnerHtml = ""
  Visual.SimInfo.Style.backgroundColor = $(ComInfoColor)
  enableButtons

End Sub            
