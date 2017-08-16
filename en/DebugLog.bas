
'**********************************************************************
'* Purpose: Create a logging window to log messages
'* Input:  none
'* Output: none
'**********************************************************************
Function CreateDebugLogWindow()
  Dim DebugLogWindow

  Set DebugLogWindow = CreateObject("WinAPI.Window")
  DebugLogWindow.Title = "XMF Debug Log"
  Memory.Set"DebugLogWindow", DebugLogWindow
End Function
'+++++++++++++++++++++ End of CreateDebugLogWindow() +++++++++++++++++++++++

'************************************************************************
'* Purpose: Display message log on Caccia log window        *
'* Input:  sMessage                   *
'* Output: None                    *
'************************************************************************
Function DebugMessage(sMessage)
  Dim sTimeStamp, sDate, sTime, DebugLogWindow

  If Memory.Get("DebugLogWindow", DebugLogWindow) Then
    If Len(sMessage) <> 0 Then
      sDate = Convert.FormatTime(System.Time, "%d-%m-%Y")
      sTime = String.Format("%02d:%02d:%02d", Hour(Time), Minute(Time), Second(Time))
      sTimeStamp = sDate & " " & sTime & " "
      DebugLogWindow.Log(sTimeStamp & sMessage & vbCrLf)
    End If
  End If
End Function
' +++++++++++++++++++ End of DebugMessage() ++++++++++++

Function DebugWindowClose
  Dim DebugLogWindow
  If Memory.Get("DebugLogWindow", DebugLogWindow) Then
    DebugLogWindow.DeleteContents
    'DebugLogWindow.Quit
    Memory.Free "DebugLogWindow"
  End If
End Function