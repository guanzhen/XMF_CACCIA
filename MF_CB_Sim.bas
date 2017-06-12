'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------


#define CMD_TIMEOUT    2000

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Simulate control board commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub sendCommand (cmdIndex)
  Dim senddata(), recData()
  Dim dataLen, i, cs, x
  Dim dummyData()
  Dim hlp, hlp1
  
  Visual.SimInfo.InnerHtml = "SEND MESSAGE"
  Visual.SimInfo.Style.backgroundColor = $(MessageColor)  
  dataLen = 2 + String.SafeParse(Visual.Script("CommandTableGrid").getVal(cmdIndex,2), 0)
  ReDim senddata(dataLen+2)
  
  For i = 0 to dataLen
    cs = Visual.Script("CommandTableGrid").getColspan(cmdIndex,i+1)
    If cs > 0 then
      hlp = Visual.Script("CommandTableGrid").getVal(cmdIndex,i+1)
      If InStr(1,hlp,"x",1) = 0 Then
        hlp1 = Math.CastFloat2Long(String.SafeParse(hlp, 0.0))
      else
        hlp1 = String.safeParse(hlp,0)        
      End If
      DebugMessage "hlp11=" & hlp1
      for x = 0 to cs-1
        senddata(i+x) = Lang.GetByte(hlp1,x)
      Next
      i = i + cs-1
    Else
      hlp1 = Visual.Script("CommandTableGrid").getVal(cmdIndex,i+1)
      DebugMessage "hlp12=" & hlp1
      'Bug in IsNumeric function: it cannot detect 'L'
      If (Lang.IsNumeric(hlp1) = True) AND NOT (hlp1 = "L") Then
        DebugMessage "hlp12=num"
        senddata(i) = String.SafeParse ( Visual.Script("CommandTableGrid").getVal(cmdIndex,i+1), 0)
        senddata(i) = Lang.GetByte(senddata(i),0)
      Else
        senddata(i) = Asc(Visual.Script("CommandTableGrid").getVal(cmdIndex,i+1))
      End If
    End If
    DebugMessage "sendata(i)=" & senddata(i)
  Next
  ClearComInputbuffer
  sendComMessage sendData , dataLen
  '~~~ Wait for acknowledge ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  wait4Ack $(CMD_ACK), 0, dummyData
  Visual.SimInfo.InnerHtml = ""
  Visual.SimInfo.Style.backgroundColor = $(ComInfoColor)
End Sub



Sub CBCmdTable_RightClick (obj, rowId, index, obj2)
  Dim CmdIndex
  
  CmdIndex = Visual.Script("CommandTableGrid").getRowIndex (rowId )
  Visual.Script("CommandTableGrid").SelectRow(CmdIndex)
  sendCommand CmdIndex
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Wait for acknowledge ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function wait4Ack (mode, expAckLen, byRef ackData)
  Dim recData(), dataLen
  Dim rowsNum, i, cs
  
  wait4Ack = $(ACK_OK)
  If getComMessage ($(CMD_TIMEOUT), recData, dataLen, True) = $(ACK_OK) Then
    If mode = $(CMD_ACK) Then
      cs = 0
      Window.LockWindowUpdate true   
      rowsNum = Visual.Script("AckTableGrid").getRowsNum()
      If rowsNum > $(MAX_ROWS) Then
        Visual.Script("AckTableGrid").DelRow rowsNum-1
      End If
      Visual.Script("AckTableGrid").AddRow(0)
      Visual.Script("AckTableGrid").setVal 0,0,getAckSignification(recData(0))
      For i = 0 To dataLen-2
        Visual.Script("AckTableGrid").setVal 0,i+1,String.Format("0x%02X",recData(i))
        cs = cs - recData(i)
      Next
      For i = dataLen-1 to 11
        Visual.Script("AckTableGrid").setVal 0,i+1," "
      Next
      Visual.Script("AckTableGrid").setVal 0,12,String.Format("0x%02X",recData(dataLen-1))
      Visual.Script("AckTableGrid").SelectRow 0
      If Not Lang.LoByte(cs) = recData(dataLen-1) Then
        Visual.Script("AckTableGrid").setVal 0,0, "Wrong CS - " & String.Format("0x%02X",Lang.LoByte(cs)) & " expected"
        Visual.Script("AckTableGrid").SetRowTextRed 0, "red"
      End If
      If Not recData(2) = 0 Then
        Visual.Script("AckTableGrid").SetRowTextRed 0, "red"
      End if
      Visual.Script("DecoderGrid").clearAll
      Visual.DecoderInfo.InnerHtml = ""
      DecodeMB_Ack 0
      Window.LockWindowUpdate false
    Else
      If Not dataLen = expAckLen and Not recData(2) = $(ACK_NO_MORE_DATA) Then
        System.MessageBox "~cf3 Wrong acknowledge length - expected:" & expAckLen & "  received:" & dataLen & " !", "Communication error", MB_ICONERROR 
        wait4Ack = $(ACK_NOK)
      ElseIf Not recData(2) = $(ACK_OK) and Not recData(2) = $(ACK_NO_MORE_DATA) Then
        System.MessageBox "~cf3 Acknowledge error: " & decodeAckState(String.Format("0x%02x",recData(2))) & " !", "Communication error", MB_ICONERROR 
        wait4Ack = $(ACK_NOK)
      Else
        cs = 0
        ReDim ackData(dataLen+8)
        For i = 0 To dataLen-2
          ackData(i) = recData(i)
          cs         = cs - recData(i)
        Next
        If Not Lang.LoByte(cs) = recData(dataLen-1) Then
          System.MessageBox "~cf3 Checksum error !", "Communication error", MB_ICONERROR 
          wait4Ack = $(ACK_NOK)
        End If
      End If
    End If
  Else
    System.MessageBox "~cf3Acknowledge timeout !", "Communication error", MB_ICONHAND
    wait4Ack = $(ACK_NOK)
  End If
End Function





'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Decode acknowledge message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub CBAckTable_RightClick (obj, rowId, index, obj2)
  Dim CmdIndex
  Visual.Script("DecoderGrid").clearAll
  Visual.DecoderInfo.InnerHtml = ""
  CmdIndex = Visual.Script("AckTableGrid").getRowIndex (rowId )
  Visual.Script("AckTableGrid").SelectRow(CmdIndex)
  DecodeMB_Ack CmdIndex
End Sub

Sub DecodeMB_Ack (row)
  Dim command, xmlOk, found
  Dim ackNode, i, x, Line, Ack
  Dim convStr
  
  command = Visual.Script("AckTableGrid").getVal(row,0)
  If File.FileExists(System.Environment.Path & $(DecoderXml) ) = False Then
    System.MessageBox "~cf3 File " & Chr(34) & $(DecoderXml) & Chr(34) & " does not exist !", "File error", MB_ICONERROR 
  Else
    xmlOk = True
    Set ackNode = CreateObject("XMLCW.XmlParser").Build( System.Environment.Path & $(DecoderXml) )
    xmlOk = Lang.IsObject(ackNode)
    If xmlOk = True Then
      Set ackNode = ackNode("Acknowledges").SelectNodes("ack")
      xmlOk = Lang.IsObject(ackNode)
    End If
    If xmlOk = True Then
      found = False
      For i = 0 to ackNode.Size-1
        Set Line = ackNode(i).MatchChildContent("Line", command)
        If Lang.IsObject(Line) Then
          Set ackNode = ackNode(i).SelectNodes("Line")
          found = True
          Visual.DecoderInfo.InnerHtml = "A C K N O W L E D G E"
          For x = 0 to ackNode.Size-1
            Visual.Script("DecoderGrid").addRow
            Visual.Script("DecoderGrid").setVal x,0,ackNode.ChildContent(x)
            convStr = ConvertValue(ackNode,x,row,Visual.Script("AckTableGrid") )
            Visual.Script("DecoderGrid").setVal x,1, convStr
            If ackNode.ChildContent(x) = "STATE" Then
              If convStr = decodeAckState("0x00") Then
                Visual.Script("DecoderGrid").SetRowTextBlack 1
              Else
                Visual.Script("DecoderGrid").SetRowTextRed 1
              End If
            End If
          Next
          Exit For
        End If
      Next
      If found = False Then
        Visual.Script("DecoderGrid").addRow
        Visual.Script("DecoderGrid").setVal 0,0,"Unknown acknowledge"
      End If
    End If
    If xmlOk = False Then
      System.MessageBox "~cf3 XML format error !", "File error", MB_ICONERROR 
    End If
  End If
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Get  acknowledge signification according to ack code ~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function getAckSignification  (ack)
  Dim ackVal, xmlOk, ackNode, Line
  Dim i
  
  getAckSignification = "Unknown acknowledge code"
  ackVal = String.Format("0x%02X",ack)
  If File.FileExists(System.Environment.Path & $(AckTable) ) = False Then
    System.MessageBox "~cf3 File " & Chr(34) & $(AckTable) & Chr(34) & " does not exist !", "File error", MB_ICONERROR 
  Else
    xmlOk = True
    Set ackNode = CreateObject("XMLCW.XmlParser").Build( System.Environment.Path & $(AckTable) )
    xmlOk = Lang.IsObject(ackNode)
    If xmlOk = True Then
      For i = 0 to ackNode.Size-1
        If ackNode(i).ChildContent(2) = ackVal Then
          getAckSignification = ackNode(i).ChildContent(0)
          Exit For
        End If
      Next
    End If
  End If
End Function 


 
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Decode acknowledge state ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function decodeAckState (stateStr)
  Select case stateStr
    case "0x00"  decodeAckState = "OK"
    case "0x01"  decodeAckState = "NOK"
    case "0x02"  decodeAckState = "UNKOWN_CMD"
    case "0x03"  decodeAckState = "NOT_IMPLEMENTED"
    case "0x04"  decodeAckState = "UNKNOWN_TARGET"
    case "0x05"  decodeAckState = "INVALID_PARAM"
    case "0x06"  decodeAckState = "WRONG_LENGHT"
    case "0x40"  decodeAckState = "DEST_FW_NOT_EXIST"
    case "0xAA"  decodeAckState = "ACTION_NOT_POSSIBLE"
    case "0x10"  decodeAckState = "NO_MORE_DATA"
    case "0x14"  decodeAckState = "ERR_WRONG_MEM_ADRESS"
    case "0x15"  decodeAckState = "ERR_WRONG_MEM_TARGET"
    case "0xD0"  decodeAckState = "CRC_ERROR"
    case "0xD1"  decodeAckState = "CM_NOT_CONNECTED"
    case "0xD2"  decodeAckState = "WRONG_POLARITY"
    case "0xD3"  decodeAckState = "MAX_VOLTAGE"
  End Select
End function

Function decodeTarget (stateStr)
  Select case stateStr
    case "0x00"  decodeTarget = "CB_BIOS"
    case "0x10"  decodeTarget = "CB_APP"
    case "0x20"  decodeTarget = "MB_BOOTLOADER"
    case "0x30"  decodeTarget = "MB_APP"
  End Select
End function

















