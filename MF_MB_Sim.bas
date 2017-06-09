'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Simulate control board commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sub OnClick_BUTTON_STARTREC(Reason)
  Dim stRec 
  Dim recData, lendata, sLendata, senddata()
  Dim i, x, y, cs, ackLineNo, rowsNum
  Dim cmd, ackNode, xmlOk
  Dim displayData, unknownCmd
  
  stRec       = False
  displayData = False
  Memory.Set "stopRec", False
  Window.LockWindowUpdate true   
  Visual.BUTTON_STARTREC.Disabled = True
  Visual.BUTTON_STOPREC.Disabled = False
  disableButtons
  Visual.WaitCommand.Style.display = "block"
  Visual.Script("ji").MakeERunVisible
  Visual.SimInfo.InnerHtml = "MB Simulation ON"
  Visual.SimInfo.Style.backgroundColor = $(SimInfoColor)
  Window.LockWindowUpdate false
  ClearComInputbuffer
  Do While Memory.stopRec = False
    If getComMessage (100, recData, lenData, False) = $(ACK_OK) Then
      Window.LockWindowUpdate true  
      If recData(0) <= &h04 Then
        ' Download ack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ReDim senddata(6)
        senddata(0) = recData(0)
        If recData(0) = $(DOWNLOAD_SOF) Then
          Visual.WaitCommand.Style.display = "none"
          Visual.DownloadIcon.Style.display = "block"
          Visual.DownloadAnim.Style.display = "block"
          senddata(1) = 2
          senddata(2) = 0
          senddata(3) = recData(2)
          lendata     = 4
        ElseIf recData(0) = $(DOWNLOAD_EOF) Then
          Visual.WaitCommand.Style.display  = "block"
          Visual.DownloadIcon.Style.display = "none"
          Visual.DownloadAnim.Style.display = "none"
          senddata(1) = 1
          senddata(2) = 0
          lendata     = 3
        Else
          senddata(1) = 1
          senddata(2) = 0
          lendata     = 3
        End If
        sendAckMessageOptRandAckErr sendData , lendata
      ElseIf RecData(0) = $(CMD_GET_DATA) And RecData(2) < &h20 Then
        ' Memory ack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        If RecData(2) = $(PARAM_GET_MEMORY_START) Or RecData(2) = $(PARAM_GET_MEMORY_END) Then
          If RecData(2) = $(PARAM_GET_MEMORY_START) Then
            Visual.WaitCommand.Style.display    = "none"
            Visual.ReadMemoryIcon.Style.display = "block"
            Visual.DownloadAnim.Style.display   = "block"
          End If
          If RecData(2) = $(PARAM_GET_MEMORY_END) Then
            Visual.WaitCommand.Style.display    = "block"
            Visual.ReadMemoryIcon.Style.display = "none"
            Visual.DownloadAnim.Style.display   = "none"
          End If
          ReDim senddata(6)
          ' Memory start or Memory end ~~~~~~~~~~
          senddata(0) = $(CMD_GET_DATA)
          ' Len ~~~~~~~~~
          senddata(1) = 1
          ' State ~~~~~~~
          senddata(2) = 0
          lendata     = 3
          sendAckMessageOptRandAckErr sendData , lendata
        ElseIf RecData(2) = $(PARAM_GET_MEMORY_LINE) Then
          ReDim senddata(24)
          senddata(0) = $(CMD_GET_DATA)
          ' Len ~~~~~~~~~
          senddata(1) = 17
          ' State ~~~~~~~
          senddata(2) = 0
          For i = 3 to 18
            senddata(i) = CInt( 200 * Rnd + 1 )
          Next
          lendata     = 19
          sendAckMessageOptRandAckErr sendData , lendata
        ElseIf RecData(2) = $(PARAM_MEMORY_SINGLE) Then
          Visual.Script("ji").MakeERunHidden
          ReDim senddata(24)
          senddata(0) = $(CMD_GET_DATA)
          ' Len ~~~~~~~~~
          senddata(1) = 5
          ' State ~~~~~~~
          senddata(2) = 0
          For i = 3 to 6
            senddata(i) = CInt( 200 * Rnd + 1 )
          Next
          lendata     = 7
          sendAckMessageOptRandAckErr sendData , lendata
        Else
          Visual.Script("ji").MakeERunHidden
          ' Invalid param ~~~~~~~~~~~~~~~~~~~~~~~~
          senddata(0) = $(CMD_GET_DATA)
          senddata(1) = 1
          senddata(2) = $(ACK_INVALID_PARAM)
          lendata     = 3
          sendAckMessageOptRandAckErr sendData , lendata
          Visual.Script("ji").MakeERunVisible
        End If
      ElseIf RecData(0) = $(CMD_SEND_DATA) And RecData(2) = $(PARAM_MEMORY_SINGLE) Then
        Visual.Script("ji").MakeERunHidden
        ' Write data single ~~~~~~~~~~~~~~~~~~~~~~
        senddata(0) = $(CMD_SEND_DATA)
        senddata(1) = 1
        senddata(2) = 0
        lendata     = 3
        sendAckMessageOptRandAckErr sendData , lendata
        Visual.Script("ji").MakeERunVisible
      Else
        System.Start "indicateNewCmd"
        displayData = True
        cs = 0
        cmd = getCmdSignification(recData(0),ackLineNo)
        For i = 0 To lenData-2
          cs = cs - recData(i)
        Next
        If Not Lang.LoByte(cs) = recData(lendata-1) Then
          Visual.Script("CmdTableGrid").setVal 0,0, "Wrong CS - " & String.Format("0x%02X",Lang.LoByte(cs)) & " expected"
          Visual.Script("CmdTableGrid").SetRowTextRed 0, "red"
        Else
          If Not ackLineNo = -1 Then
            unknownCmd = False
            '~~~ Send appropriate acknowledge ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            Set ackNode = CreateObject("XMLCW.XmlParser").Build( System.Environment.Path & $(ACK_WORK_FILE) )
            xmlOk = Lang.IsObject(ackNode)
            If xmlOk = True Then
              For i = 0 to ackNode.Size-1
                DebugMessage "Node:" & i
                If ackNode(i).ChildContent(0) = cmd Then
                  DebugMessage "Cmd = " & cmd
                  If ackNode(i).ChildContent(1) > 0 Then
                    ' Delay
                    System.Delay ackNode(i).ChildContent(1)
                  End If
                  sLendata = 2 + String.SafeParse(ackNode(i).ChildContent(3), 0)
                  DebugMessage "Sendlen = " & sLendata
                  ReDim senddata(sLendata+2)
                  Dim hlp,hlp1
                  For x = 2 to sLendata+1
                    hlp = ackNode(i).ChildContent(x)
                    DebugMessage "hlp = " & hlp
                    cs = String.SafeParse(ackNode(i).Child(x).Attribute.ItemContent("colspan"), 0)                    
                    DebugMessage "x =" & x &" " &cs
                    If cs > 0 Then
                      If InStr(1,hlp,"x",1) = 0 Then
                        hlp1 = Math.CastFloat2Long(String.SafeParse(hlp, 0.0))
                      else
                        hlp1 = String.safeParse(hlp,0)
                      End If
                      for y = 0 to cs-1
                        senddata(x-2+y) = Lang.GetByte(hlp1,y)
                      Next
                      x = x + cs-1
                    Else
                      If Lang.IsNumeric(hlp) = True Then
                        hlp1 = String.SafeParse(hlp, 0)
                        If hlp1 < 0 Then
                          senddata(x-2) = Lang.GetByte(hlp1,0)
                        Else
                          senddata(x-2) = hlp1
                        End If
                      Else
                        senddata(x-2) = Asc(hlp)
                      End If
                    End If
                  Next
                  Exit For
                End If
              Next
            End If
            '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          Else
            unknownCmd = True
            ReDim senddata(4)
            senddata(0) = recData(0)
            senddata(1) = 1
            senddata(2) = $(ACK_UNKOWN_CMD)
            sLendata = 3
          End If
          sendAckMessageOptRandAckErr sendData , sLendata
          If displayData = True Then
            rowsNum = Visual.Script("CmdTableGrid").getRowsNum()
            If rowsNum > $(MAX_ROWS) Then
              Visual.Script("CmdTableGrid").DelRow rowsNum-1
            End If
            Visual.Script("CmdTableGrid").AddRow(0)
            Visual.Script("CmdTableGrid").setVal 0,0, cmd, ackLineNo
            For i = 0 To lenData-2
              Visual.Script("CmdTableGrid").setVal 0,i+1,String.Format("0x%02X",recData(i))
            Next
            For i = lenData-1 to 11
              Visual.Script("CmdTableGrid").setVal 0,i+1," "
            Next
            Visual.Script("CmdTableGrid").setVal 0,12,String.Format("0x%02X",recData(lenData-1))
            Visual.Script("CmdTableGrid").SelectRow 0
            displayData = False
            If unknownCmd = True Then
              Visual.Script("CmdTableGrid").SetRowTextRed 0, "red"
            End If
          End If
        End If
      End If
      Window.LockWindowUpdate false
    Else
      If Memory.dispLog = True Then
        System.Log "."
      End If    
    End If
  Loop
  Window.LockWindowUpdate true
  Visual.BUTTON_STARTREC.Disabled = False
  Visual.BUTTON_STOPREC.Disabled = True
  Visual.Script("ji").MakeERunHidden
  Visual.WaitCommand.Style.display  = "none"
  Visual.DownloadIcon.Style.display = "none"
  Visual.DownloadAnim.Style.display = "none"
  Visual.SimInfo.InnerHtml = ""
  Visual.SimInfo.Style.backgroundColor = $(ComInfoColor)
  Window.LockWindowUpdate false   
End Sub

Sub OnClick_BUTTON_STOPREC(Reason)
  enableButtons
  Memory.Set "stopRec", True
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Get command signification according to cmd code ~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function getCmdSignification  (cmd, byRef LineNo)
  Dim cmdVal, xmlOk, ackNode, Line
  Dim i
  
  getCmdSignification = "Unknown command code"
  LineNo = -1
  cmdVal = String.Format("0x%02X",cmd)
  If File.FileExists(System.Environment.Path & $(CmdTable) ) = False Then
    getCmdSignification = "XML-File does not exist"
    System.MessageBox "~cf3 File " & Chr(34) & $(CmdTable) & Chr(34) & " does not exist !", "File error", MB_ICONERROR 
  Else
    xmlOk = True
    Set ackNode = CreateObject("XMLCW.XmlParser").Build( System.Environment.Path & $(CmdTable) )
    xmlOk = Lang.IsObject(ackNode)
    If xmlOk = True Then
      For i = 0 to ackNode.Size-1
        If ackNode(i).ChildContent(1) = cmdVal Then
          getCmdSignification = ackNode(i).ChildContent(0)
          LineNo = i
          Exit For
        End If
      Next
    End If
  End If
End Function 

'~~~ Clear grid completely ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnClick_BUTTON_CLEAR_REC_GRID(Reason)
  Visual.Script("CmdTableGrid").clearAll
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Decode command message ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub MBCmdTable_RightClick (obj, rowId, index, obj2)
  Dim CmdIndex
  Visual.Script("DecoderGrid").clearAll
  Visual.DecoderInfo.InnerHtml = ""
  CmdIndex = Visual.Script("CmdTableGrid").getRowIndex (rowId )
  Visual.Script("CmdTableGrid").SelectRow(CmdIndex)
  DecodeCB_Cmd CmdIndex
End Sub


Sub DecodeCB_Cmd (row)
  Dim command, xmlOk, found
  Dim ackNode, i, x, Line, Ack
  
  command = Visual.Script("CmdTableGrid").getVal(row,0)
  If File.FileExists(System.Environment.Path & $(DecoderXml) ) = False Then
    System.MessageBox "~cf3 File " & Chr(34) & $(DecoderXml) & Chr(34) & " does not exist !", "File error", MB_ICONERROR 
  Else
    xmlOk = True
    Set ackNode = CreateObject("XMLCW.XmlParser").Build( System.Environment.Path & $(DecoderXml) )
    xmlOk = Lang.IsObject(ackNode)
    If xmlOk = True Then
      Set ackNode = ackNode("Commands").SelectNodes("cmd")
      xmlOk = Lang.IsObject(ackNode)
    End If
    If xmlOk = True Then
      found = False
      For i = 0 to ackNode.Size-1
        Set Line = ackNode(i).MatchChildContent("Line", command)
        If Lang.IsObject(Line) Then
          Set ackNode = ackNode(i).SelectNodes("Line")
          found = True
          Visual.DecoderInfo.InnerHtml = "C O M M A N D"
          For x = 0 to ackNode.Size-1
            Visual.Script("DecoderGrid").addRow
            Visual.Script("DecoderGrid").setVal x,0,ackNode.ChildContent(x)
            Visual.Script("DecoderGrid").setVal x,1,ConvertValue(ackNode,x,row,Visual.Script("CmdTableGrid") )
          Next
          Exit For
        End If
      Next
      If found = False Then
        Visual.Script("DecoderGrid").addRow
        Visual.Script("DecoderGrid").setVal 0,0,"Unknown command"
      End If
    End If
    If xmlOk = False Then
      System.MessageBox "~cf3 XML format error !", "File error", MB_ICONERROR 
    End If
  End If
End Sub



'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Write work XML file idf value changed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
' Workaround ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub MBAckTable_CellChanged (obj, rowId, index, obj2)
  System.Start "Write_MBAckTable"
End Sub

Sub Write_MBAckTable()
  Dim workFile, xmlString
  ' Write AckTable to file
  xmlString = Visual.Script("AcknowledgeTableGrid").serialize()
  xmlString = String.Replace(xmlString, "'", chr(34)&"")
  Set workFile = File.Open( System.Environment.Path & $(ACK_WORK_FILE), "wt")
  workFile.Write xmlString
  Set workFile = nothing
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Display functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub indicateNewCmd ()
  Visual.Script("ji").MakeERunHidden
  System.delay(100)
  Visual.Script("ji").MakeERunVisible
End Sub


'-------------------------------------------------------------------------------
'--- Send message to COM port with option random acknowledge error -------------
'-------------------------------------------------------------------------------

Sub initRandAckErr (  )
  ' Generate a random integer between 210 and 10 ~~~~~~~~
  Memory.Set "RndAckErrCnt", Int (Visual.RepRateAckErrInp.Value * Rnd + 1)   
End Sub

Sub OnChange_RepRateAckErrInp ( Reason )
  initRandAckErr
  Visual.RandAckErrCnt.InnerHtml        = String.Format("%03d",Memory.RndAckErrCnt)
End Sub

Sub OnClick_RandAckErrCheckBox( Reason )
  If Visual.RandAckErrCheckBox.Checked = True Then
    initRandAckErr
    Visual.RandAckErrCnt.InnerHtml        = String.Format("%03d",Memory.RndAckErrCnt)
    Visual.RandAckErrCnt.Style.display    = "block"
    Visual.RepRateAckErrTxt.Style.display = "block"
    Visual.RepRateAckErrInp.Style.display = "block"
    Visual.SelRandAckErr.Style.display    = "block"
  Else
    Visual.RandAckErrCnt.Style.display    = "none"
    Visual.RepRateAckErrTxt.Style.display = "none"
    Visual.RepRateAckErrInp.Style.display = "none"
    Visual.SelRandAckErr.Style.display    = "none"
  End If
End Sub

Sub sendAckMessageOptRandAckErr ( sendData , dataLen )
  Dim AckErr
  AckErr = False
  If Visual.RandAckErrCheckBox.Checked = True Then
    Memory.RndAckErrCnt = Memory.RndAckErrCnt -1
    If Memory.RndAckErrCnt = 0 Then
      ' Generate a random ack error between 0 and 6 ~~~~~
      sendData(2) = Visual.SelRandAckErr.SelectedItemAttribute("value")  
      initRandAckErr
      AckErr = True
    End If
    Visual.RandAckErrCnt.InnerHtml = String.Format("%03d",Memory.RndAckErrCnt) 
  End If
  sendComMessage  sendData , dataLen 

End Sub


