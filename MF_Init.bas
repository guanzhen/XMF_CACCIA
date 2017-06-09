'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
'--- ON LOAD FRAME ------------------------------------------------------------
'------------------------------------------------------------------------------
#define CONFIG_VERSION  "Configversion V1.00"
#define CONFIG_LEN      16

Dim configLoaded

Sub OnLoadFrame()
  Dim Paramfile
  Dim i, Config, Release, TitleString
  
  'log outputs

  Memory.Set "dispLog", false
  
  Window.Width  = 1040
  Window.Height = 680
  
  ' Create version and title ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  If Not System.Configuration ("Version", Config, "Package") Then
    Release = "missing"
  Else
    Release = "V " & Config.Param(0)
  End If
  If Not System.Configuration( "Description", Config, "Package") Then
    TitleString = "Missing title"
  End If
  TitleString = Config.Param(0) & "   " & Release
  Window.Title = TitleString
  ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  InitComponents
  InitHexEditCtrl
  Visual.BUTTON_STOPREC.Disabled = True    
  '~~~ Read params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ReadConfigParam
  '~~~ Disable elements for input only ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  DisableComponents
'  Visual.SerScopeGrid.AddRows 10,10, True, 1
  InitSerialScope
  InitHexEditCtrl
  
  CreateDebugLogWindow()
End Sub

Sub OnUnloadFrame()
  '~~~ Disconnect COM ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  disconnectCom
  Memory.Set "stopRec", True
  '~~~ Save params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  If configLoaded = True Then
    WriteConfigParam
  End If
  DebugWindowClose
End Sub


'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Initialize serial scope ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub InitSerialScope()
  Dim SerialScope
  Dim i, Font
  
  Set Font = CreateStdFont
  With Font
    .Name  = "courier new"
    .Size  = 7
  End With  
  Set SerialScope = Visual.SerScopeGrid.Object
  With SerialScope
    .Cols = 3
    .ColWidth(0)     = 40
    .ColWidth(1)     = 70
    .ColWidth(2)     = 200
    .ColAlign(0)     = lgAlignCenter
    .ColAlign(1)     = lgAlignCenter
    .ColAlign(2)     = lgAlignLeft
    .NormalTextColor = Color_Black
    .NormalBkColor   = Color_White
    Set .NormalFont  = Font
    .Tooltips        = True
'    for i = 0 to 10
'      .addRow "CB --> MB " & i
'    Next
'    .cellText(1,1) = "MMMMMMM"
  End With
'  MsgBox SerialScope.Rows
  Memory.Set "SerialScope", SerialScope
  Memory.Set "SerialScopeLines", 0
End Sub
'-------------------------------------------------------------------------------
'--- Initialize components -----------------------------------------------------
'-------------------------------------------------------------------------------
Sub InitComponents
  Dim line,col,i
  Dim ret
  
  Visual.Script("MF_TabBar").init()
  For i = 0 To Visual.Script("MF_TabBar").getNumberOfTabs() - 1
    With Visual.Select("TabStripPage"&i).Style
      .Height = (Visual.Select("LayerBody").Style.PixelHeight - 36) & "px"
    End With
  Next
  ' grids ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Visual.Script("SysParaGrid").initGrid 
  Visual.Script("DecoderGrid").initGrid 
  Visual.Script("CommandTableGrid").initGrid  
  Visual.Script("CommandTableGrid").SelectRow(0)
  Visual.Script("CmdTableGrid").initGrid  
  Visual.Script("AcknowledgeTableGrid").initGrid  
  Visual.Script("AckTableGrid").initGrid  
  Visual.Script("InfoGrid").initGrid
  ' attach grid callbacks ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Visual.Script("CommandTableGrid").attachEvent     "onRightClick",  Lang.GetRef("CBCmdTable_RightClick") 
  Visual.Script("AckTableGrid").attachEvent         "onRightClick",  Lang.GetRef("CBAckTable_RightClick") 
  Visual.Script("CmdTableGrid").attachEvent         "onRightClick",  Lang.GetRef("MBCmdTable_RightClick") 
  Visual.Script("AcknowledgeTableGrid").attachEvent "onCellChanged", Lang.GetRef("MBAckTable_CellChanged") 
  'Memory and Drive test slider ~~~~~~~~~~~~~~~~~~~~~
  Visual.ProgressBar.Object.setPercentage (0)

End Sub 


'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Init HEX EDIT CONTROL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnChange_SelMemType( Reason )
  If Visual.SelMemType.SelectedIndex = 1 Then
    Visual.MemoryStartAdr.Value        = &h0000
    Visual.MemoryEndAdr.Value          = &h0020
    Visual.MemDataFormat.SelectedIndex = 0 
    InitHexEditCtrl
  End If
End Sub

Sub OnChange_MemDataFormat( Reason )
  InitHexEditCtrl
End Sub

Sub InitHexEditCtrl
  Dim MaxWords, Field
  Dim StartAdr, EndAdr
  
  With Visual.Select("HexEditCtrl").Object
    .Columns         = 16
    .DigitsInData    = 2
    .ShowAscii       = True
    .ShowAddress     = True
    .DigitsInAddress = 8
    .AllowChangeSize = False
    .BackColor = Color_Beige
    .ForeColor = Color_Black
    If Visual.Select("MemDataFormat").SelectedIndex = 0 Then
      .DigitsInData  = 2
      .ShowAscii     = True
      .DataWidth     = 8
    Else
      .DigitsInData  = 4
      .ShowAscii     = False
      .DataWidth     = 16
    End If  
  End With
  checkMemoryParams
End Sub 

Sub SetHexEditCtrl( startAdr, endAdr )
  Dim MaxWords, Field, LoopCnt

  If endAdr > startAdr Then
    MaxWords = endAdr - startAdr
  Else
    MaxWords = 16
  End If
  Field = Object.SafeArrayCreate( MaxWords )
  '--- Dummy data -------------------------------------------------------------
  For loopCnt = 0 To MaxWords - 1
    Field( loopCnt ) = 0
  Next
  '----------------------------------------------------------------------------
  Visual.Select("HexEditCtrl").Object.SetData Field, startAdr
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'--- Disable components --------------------------------------------------------
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub DisableComponents
  Visual.BUTTON_STARTREC.Disabled    = True
  Visual.BUTTON_LOADHEXFILE.Disabled = True
  Visual.BUTTON_DOWNLOAD.Disabled    = True
  Visual.BUTTON_READ_MEMORY.Disabled = True
  Visual.BUTTON_READ_WORD.Disabled   = True
  Visual.BUTTON_WRITE_WORD.Disabled  = True
  Visual.DwnldTarget.Disabled        = True
  Visual.Select("SimCBTabCmd").Style.Visibility    = "Hidden"
  Visual.Select("SimCBTabRecAck").Style.Visibility = "Hidden"
  Visual.Select("SimMBTabRecCmd").Style.Visibility = "Hidden"
  Visual.Select("SimMBTabDefAck").Style.Visibility = "Hidden"

End Sub 

Sub disableButtons
  Visual.BUTTON_STARTREC.Disabled    = True
  Visual.BUTTON_LOADHEXFILE.Disabled = True
  Visual.BUTTON_DOWNLOAD.Disabled    = True
  Visual.BUTTON_READ_MEMORY.Disabled = True
  Visual.BUTTON_READ_WORD.Disabled   = True
  Visual.BUTTON_WRITE_WORD.Disabled  = True
End Sub
Sub enableButtons
  Visual.BUTTON_STARTREC.Disabled    = False
  Visual.BUTTON_LOADHEXFILE.Disabled = False
  Visual.BUTTON_DOWNLOAD.Disabled    = False
  Visual.BUTTON_READ_MEMORY.Disabled = False
  Visual.BUTTON_READ_WORD.Disabled   = False
  Visual.BUTTON_WRITE_WORD.Disabled  = False
End Sub

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'--- Enable COM-Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub EnableInterface (enable)
  If enable = True Then
    Visual.Select("SimMBTabRecCmd").Style.Visibility = "Visible"
    Visual.Select("SimMBTabDefAck").Style.Visibility = "Visible"
    Visual.Select("SimCBTabCmd").Style.Visibility    = "Visible"
    Visual.Select("SimCBTabRecAck").Style.Visibility = "Visible" 
    
    Visual.BUTTON_STARTREC.Disabled    = False
    Visual.BUTTON_LOADHEXFILE.Disabled = False
    Visual.BUTTON_DOWNLOAD.Disabled    = False
    Visual.BUTTON_READ_MEMORY.Disabled = False
    Visual.BUTTON_READ_WORD.Disabled   = False
    Visual.BUTTON_WRITE_WORD.Disabled  = False
    
  Else
    Visual.Select("SimMBTabRecCmd").Style.Visibility = "Hidden"
    Visual.Select("SimMBTabDefAck").Style.Visibility = "Hidden"
    Visual.Select("SimCBTabCmd").Style.Visibility    = "Hidden"
    Visual.Select("SimCBTabRecAck").Style.Visibility = "Hidden"  
    Visual.BUTTON_STARTREC.Disabled    = True
    Visual.BUTTON_LOADHEXFILE.Disabled = True
    Visual.BUTTON_DOWNLOAD.Disabled    = True
    Visual.BUTTON_READ_MEMORY.Disabled = True
    Visual.BUTTON_READ_WORD.Disabled   = True
    Visual.BUTTON_WRITE_WORD.Disabled  = True
  End If
End Sub  



'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'--- get reader parameter ------------------------------------------------------
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnClick_BUTTON_SET_DEVID_DEFAULT(Reason)

  Visual.Script("SysParaGrid").setVal  9,$(DevIdProdDate)
  Visual.Script("SysParaGrid").setVal 10,$(DevIdManufact)
  Visual.Script("SysParaGrid").setVal 11,$(DevIdSerNo)
  Visual.Script("SysParaGrid").setVal 12,$(DevIdMatNo)
  Visual.Script("SysParaGrid").setVal 13,$(DevIdNewPart)
  Visual.Script("SysParaGrid").setVal 14,$(DevIdFuncState)
  Visual.Script("SysParaGrid").setVal 15,$(DevIdRevState)
  
End Sub  


'-- Check line numbers of config file ------------------------------------------------
Function CheckConfLen (sPath)
  Dim oFso, oReg, sData
  Const ForReading = 1
  Set oReg = New RegExp
  Set oFso = CreateObject("Scripting.FileSystemObject")
  sData = oFso.OpenTextFile(sPath, ForReading).ReadAll
  With oReg
      .Pattern = "\r\n" 
      .Global = True
       CheckConfLen = .Execute(sData).Count + 1
  End With
  Set oFso = Nothing
End Function



Sub ReadConfigParam
  Dim Paramfile, RunTime, Str, Val, cnt, sPath, version
  Dim dummy, lineCnt
  Dim confOk
  
  confOk = True
  sPath = System.Environment.Path & "Config\MF_Config.txt"
  If Not File.FileExists(sPath) = True Then
    System.MessageBox  "Configuration file does not exist , tool-settings relocated to default !", "New configuration necessary" , MB_ICONEXCLAMATION
  Else
    If Not CheckConfLen (sPath) = $(CONFIG_LEN) Then
      System.MessageBox  "Configuration file corrupt , tool-settings relocated to default !", "New configuration necessary" , MB_ICONEXCLAMATION
    Else
      Set Paramfile = File.Open( sPath, "rt")
      version = Paramfile.ReadLine
      If Not version = $(CONFIG_VERSION) Then
        System.MessageBox  "New configuration order detected, tool-settings relocated to default !", "New configuration necessary" , MB_ICONEXCLAMATION
      Else
        ' COM params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for cnt = 1 to 5
          Visual.Script("SysParaGrid").setVal cnt,Paramfile.ReadLine
        next
        ' Download ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Visual.DwnldFile.InnerHtml         = Paramfile.ReadLine
        ' Memory ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        Visual.SelMemTarget.SelectedIndex  = Paramfile.ReadLine      
        Visual.MemDataFormat.SelectedIndex = Paramfile.ReadLine      
        Visual.SelMemType.SelectedIndex    = Paramfile.ReadLine      
        Visual.MemoryStartAdr.Value        = Paramfile.ReadLine
        Visual.MemoryEndAdr.Value          = Paramfile.ReadLine
        Visual.DwnldTarget.Value           = Paramfile.ReadLine
        Visual.RepRateAckErrInp.Value      = Paramfile.ReadLine
        Visual.SelRandAckErr.SelectedIndex = Paramfile.ReadLine
      End If

      ' Close param file ~~~~
      Set Paramfile = Nothing
    End If
  End If
  configLoaded = True
End Sub

Sub WriteConfigParam
  Dim Paramfile, Str, RunTime, cnt
  Set Paramfile = File.Open( System.Environment.Path & "Config\MF_Config.txt", "wt")

  Paramfile.Write $(CONFIG_VERSION)                                    & Chr(10)
  ' COM params ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for cnt = 1 to 5
    Paramfile.Write Visual.Script("SysParaGrid").getVal(cnt)           & Chr(10)
  next
  ' Download ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ParamFile.Write Visual.DwnldFile.InnerHtml                           & Chr(10)
  ' Memory ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ParamFile.Write Visual.SelMemTarget.SelectedIndex                    & Chr(10)
  ParamFile.Write Visual.MemDataFormat.SelectedIndex                   & Chr(10)
  ParamFile.Write Visual.SelMemType.SelectedIndex                      & Chr(10)
  ParamFile.Write Visual.MemoryStartAdr.Value                          & Chr(10)
  ParamFile.Write Visual.MemoryEndAdr.Value                            & Chr(10)
  ParamFile.Write Visual.DwnldTarget.Value                             & Chr(10)
  ParamFile.Write Visual.RepRateAckErrInp.Value                        & Chr(10)
  ParamFile.Write Visual.SelRandAckErr.SelectedIndex                   & Chr(10)
  
  ' Close param file ---------------------------------------
  Set Paramfile = Nothing
End Sub


'-------------------------------------------------------------------------------
'--- Highlight selected command in grid ----------------------------------------
'-------------------------------------------------------------------------------
Sub OnChange_SelCommand( Reason )
  Visual.Script("CommandTableGrid").SelectRow(Visual.SelCommand.SelectedIndex)
End Sub
 
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Clear scope content ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnClick_BUTTON_CLEAR_SCOPE(Reason)
  Memory.SerialScope.Clear
End Sub  

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Open tzool description ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sub OnClick_BUTTON_OpenToolSpec(Reason)

  System.OpenDocument System.Environment.Path & "Doc\CacciaTool_MF.pdf", "CAdobeReaderDocument"

End Sub

