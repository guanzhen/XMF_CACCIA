'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------

'-------------------------------------------------------------------------------
'--- Open / Close COM interface ------------------------------------------------
'-------------------------------------------------------------------------------
Function connectCom 
  Dim Com, Baud, Par, StpB, DataB
  Dim ComManager
  Set ComManager = CreateObject( "ICOM.ComEntity" )
  
  Visual.Script("SysParaGrid").expAll ()
  Com   = Visual.Script("SysParaGrid").getVal (1)
  Baud  = Visual.Script("SysParaGrid").getVal (2)
  Par   = Visual.Script("SysParaGrid").getVal (3)
  StpB  = Visual.Script("SysParaGrid").getVal (4)
  DataB = Visual.Script("SysParaGrid").getVal (5)
  If ComManager.Open(Com, Baud, Par, StpB, DataB) Then
    Visual.Select("BUTTON_OPEN_CLOSE_DEVICE").Value = "Close COM device"
    Visual.Select("BUTTON_OPEN_CLOSE_DEVICE").Style.Color = "orangered"
    Memory.Set "ComManager", ComManager
    ComManager.EnableReceiveBuffer = True
    Visual.Select("connectState").Src = "icon/connect.gif"
'    System.MessageBox "Connected to device " & Com , "Connection established", MB_ICONINFORMATION
    connectCom = $(ACK_OK)
  Else
    Set Memory.ComManager = Nothing
    'Memory.Free "ComManager"
    Visual.Select("connectState").Src = "icon/disconnect.gif"
    System.MessageBox "Could not open device " & Com, "Connection Error", MB_ICONHAND
    connectCom = $(ACK_NOK)
  End If
End Function

Sub disconnectCom 

  Set Memory.ComManager = Nothing
'  Memory.Free "ComManger"
  Visual.Select("BUTTON_OPEN_CLOSE_DEVICE").Style.Color = "darkgreen"
  Visual.Select("BUTTON_OPEN_CLOSE_DEVICE").Value = "Open COM device"
  Visual.Select("connectState").Src = "icon/disconnect.gif"
End Sub

Sub OnClick_BUTTON_OPEN_CLOSE_DEVICE(Reason)

  If Visual.Select("BUTTON_OPEN_CLOSE_DEVICE").Value = "Open COM device" Then
    If connectCom = $(ACK_OK) Then
      enableInterface True
      Write_MBAckTable
    End If
  Else
    Memory.Set "stopRec", True
    System.Delay (100)
    enableInterface False
    disconnectCom
  End If
End Sub



'-------------------------------------------------------------------------------
'--- Send message to COM port --------------------------------------------------
'-------------------------------------------------------------------------------
Sub sendComMessage ( sendData , dataLen )
  Dim message (), DispMessage
  Dim cs, loopCnt, x, i
  Dim ComManager

  ReDim message(dataLen)
  Memory.Get "ComManager", ComManager
  ' First empty receive buffer
  ComManager.resetReceiveBuffer
  cs = 0
  For i = 0 to dataLen-1
    message(i) = sendData(i)
    cs = cs - message(i)
  Next
  message(i) = Lang.LoByte(cs)
  ComManager.Write message  
  
  '~~~ Serial scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Memory.SerialScope.addRow "Send"
  DispMessage = ""
  For loopCnt = 0 to dataLen
    DispMessage = DispMessage & String.Format("%02x ",message(loopCnt))
  Next
  x = Memory.SerialScope.Rows
  Memory.SerialScope.cellText(x-1,1) = String.Format("%.3f",((System.TimerHR/1000.0)+(dataLen*10*(1/115000))) )
  Memory.SerialScope.cellText(x-1,2) = DispMessage
  If x > $(MAX_LINE_SERIAL_SCOPE) Then
    Memory.SerialScope.RemoveRow (0)
  End If
  '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
End Sub 

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ clear input buffer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub ClearComInputbuffer
  Memory.ComManager.resetReceiveBuffer
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ get message from COM port ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function getComMessage (timeout, byref messageData, byref lenData, dispImmediate)
  Dim ComManager
  Dim receivedBytes, tioCnt
  Dim recData1, recData2, lenData1, lenData2, expData
  Dim i, x, recTime
  

  getComMessage = $(ACK_OK)
  Memory.Get "ComManager", ComManager
  lenData1      = 0
  lenData2      = 0
  receivedBytes = 0
  tioCnt        = 0
  Do While receivedBytes  < 2
    receivedBytes = ComManager.ReceiveBufferSize
    If tioCnt > timeout Then
      getComMessage = $(ACK_NOK)
      Exit Do
    End If
    System.Delay 1
    tioCnt = tioCnt + 1
  Loop
  recTime = System.TimerHR
  If getComMessage = $(ACK_OK) Then
    If Memory.dispLog = True Then
      System.Log String.Format("\nReceive buffer size: %d ", ComManager.ReceiveBufferSize)    
    End If
    ' read available data of message ~~~~~~~~~~~~
    If ComManager.Read (recData1, 1) = False Then
      MsgBox "! Com manager read error !"
      getComMessage = $(ACK_NOK)
    End If
  End If
  If getComMessage = $(ACK_OK) Then
    lenData1 = UBound(recData1)+1
    expData = recData1(1) + 3
    If  Not lenData1 = expdata Then
      ' Wait for message is complete ~~~~~~~~~~~~
      If Memory.dispLog = True Then
        System.Log String.Format("\n Wait for message is complete, already received: %d / expected: %d\n", lenData1, recdata1(1)+3)    
      End If
      receivedBytes = 0
      expData       = recData1(1) + 3 - lenData1
      Do While Not receivedBytes = expData
        receivedBytes = ComManager.ReceiveBufferSize
        If Memory.dispLog = True Then
          System.Log String.Format("%d+%d - tioCnt: %d / ", receivedBytes,expData,tioCnt )    
        End If
        If tioCnt > timeout Then
          getComMessage = $(ACK_NOK)
          Exit Do
        End If
        System.Delay 1
        tioCnt = tioCnt + 1
      Loop
      If receivedBytes > 0 Then
        ComManager.Read recData2, 0
        lenData2  = UBound(recData2)+1
      End If
    End If
  End If
    
  If getComMessage = $(ACK_OK) Then
    lendata = lenData1 + lendata2
    getComMessage = $(ACK_OK)
    If Memory.dispLog = True Then
      System.Log String.Format("\n Message received, len = %d ", lenData)    
    End If
    Redim messageData(lenData+2)
    For i = 0 To lenData1-1
      messageData(i) = recData1(i)
    Next
    For i = 0 To lenData2-1
      messageData(i + lenData1) = recData2(i)
    Next
    If Memory.dispLog = True Then
      System.Log String.Format("\n Message received: ")
      For i = 0 To lenData-1
        System.Log String.Format("0x%02x ", messageData(i))
      Next
    End If
    '~~~ Serial scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    If dispImmediate = False Then
      Window.LockWindowUpdate True   
    End If
    Dim DispMessage
    Memory.SerialScope.addRow "Rec"
    DispMessage = ""
    For i = 0 to lenData-1
      DispMessage = DispMessage & String.Format("%02x ",messageData(i))
    Next
    x = Memory.SerialScope.Rows
    Memory.SerialScope.cellText(x-1,1) = String.Format("%.3f",(recTime/1000.0))
    Memory.SerialScope.cellText(x-1,2) = DispMessage
    If x > $(MAX_LINE_SERIAL_SCOPE) Then
      Memory.SerialScope.RemoveRow (0)
    End If
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  End If
End Function

