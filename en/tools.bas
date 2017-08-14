'------------------------------------------------------------------------------
'--    ___   _      ___   ___  _____  _      ___     _____            _      --
'--   / __\ /_\    / __\ / __\ \_   \/_\    ( _ )   /__   \___   ___ | |___  --
'--  / /   //_\\  / /   / /     / /\//_\\   / _ \/\   / /\/ _ \ / _ \| / __| --
'-- / /___/  _  \/ /___/ /___/\/ /_/  _  \ | (_>  <  / / | (_) | (_) | \__ \ --
'-- \____/\_/ \_/\____/\____/\____/\_/ \_/  \___/\/  \/   \___/ \___/|_|___/ --
'--                                                                          --
'------------------------------------------------------------------------------

'---------------------------------------------------------------------
'*** Check a value for is a numeric **********************************
'---------------------------------------------------------------------
Function isNumeric (checkValue, byRef actVal, errTxt, checkZero)
  Dim isNum, num, val

  isNumeric = True
  val = Visual.Select(checkValue).Value
  val = trim(val)
  If Len(val) = 0 Then
    isNumeric = False
  Else
    isNumeric = Lang.IsNumeric(val)
    If isNumeric = True Then actval = val End If
  End If
  If isNumeric = False Then
    Visual.Select(checkValue).Style.BackgroundColor = "red"
    System.MessageBox  Chr(34) & errTxt & Chr(34) & _
                       " is not a number, please check input !", "NaN" , MB_ICONEXCLAMATION
    Visual.Select(checkValue).Style.BackgroundColor = ""
  Else
    If CheckZero = True And actVal = 0 Then
      isNumeric = False
      Visual.Select(checkValue).Style.BackgroundColor = "red"
      System.MessageBox  Chr(34) & errTxt & Chr(34) & _
                         " is zero, please check input !", "NaN" , MB_ICONEXCLAMATION
      Visual.Select(checkValue).Style.BackgroundColor = ""
    End If
  End If
End Function

'---------------------------------------------------------------------
'*** HIGHLIGHT BUTTON ON/OFF******************************************
'---------------------------------------------------------------------

Sub HighlightButton (Button, state)
  If state = True Then 
    Visual.Select(Button).Style.BorderColor = "lime"
    Visual.Select(Button).Style.TextDecoration = "underline"
    Visual.Select(Button).Disabled = True
  Else 
    Visual.Select(Button).Disabled = False
    Visual.Select(Button).Style.BorderColor = ""
    Visual.Select(Button).Style.TextDecoration = ""
  End If 
End Sub



'---------------------------------------------------------------------
'*** Write "Value to an array ****************************************
'---------------------------------------------------------------------
Function writeString( Field, index , num, strVal, default)
  Dim i, x, strLen, deltaLen
  
  If strVal = "&nbsp;" Then
    strVal = " "
  End If
  strLen = Len(strVal)
  If strLen > num Then
    strLen=num
  End If 
   If strlen < num Then 
     deltaLen = num - strlen
   Else 
     deltaLen = 0
  End If 
  i = 0
  x = 1
  Do while i < num
    If deltaLen > 0 And i >= (num-deltaLen) Then  
      Field(index+i) = default
    Else 
      Field(index+i) = Asc(Mid(strVal,x,1))
      x = x + 1
    End If 
    i=i+1
  Loop

End Function

'========================================================================================
' CreateDotFormatedString
'
' returns the char key for the production month
' attention: October is represented by a O - "Oh" ( NOT BY 0 - "Null")
'========================================================================================
Function CreateDotFormatedString (longVal)
  Dim i, hlpStr, hlpVal, loops

  if longVal > 999999999 then
    loops = 3
  elseif longVal > 999999 then
    loops = 2
  elseif longVal > 999 then
    loops = 1
  else
    loops = 0
    CreateDotFormatedString = String.Format("%3d", longVal Mod 1000)
  end if 
  if loops > 0 then
    hlpVal = CLng(longVal)
    hlpStr = ""
    for i = 1 to loops
      hlpStr = String.Format(".%03d", (hlpVal Mod 1000)) & hlpStr
      hlpVal = hlpVal \ 1000
    next
    CreateDotFormatedString = String.Format("%d", (hlpVal Mod 1000)) & hlpStr
  end if 
end function

'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'~~~ Convert acknowledge values acoording to identifier inside xml ~~~~~~~~~~~~~
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Function ConvertValue (node, index, line, gridObj)
  Dim col, lb, hb
  col = CInt(node.Child(index).Attribute.Attribute("byteIndex")) + 1
  Select case node.Child(index).Attribute.Attribute("format")
    case "state" ConvertValue = decodeAckState(gridObj.getVal(line,col))
    case "c"     ConvertValue = gridObj.getVal(line,col)
    case "x"     ConvertValue = gridObj.getVal(line,col)
    case "a"     ConvertValue = String.Format( "%c", String.SafeParse(gridObj.getVal(line,col),0 ) )
    case "s"     ConvertValue = Lang.MakeWord( String.SafeParse(gridObj.getVal(line,col),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+1),0) )
    case "exp"   ConvertValue = "E" & String.Format("%+06G",Math.Cast2Byte(String.SafeParse(gridObj.getVal(line,col),0) ) )
    case "sl"    ConvertValue = Lang.MakeLong4(String.SafeParse(gridObj.getVal(line,col),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+1),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+2),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+3),0) )
    case "f"     ConvertValue = String.Format("%G",Math.CastLong2Float(Lang.MakeLong4( _
                                               String.SafeParse(gridObj.getVal(line,col),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+1),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+2),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+3),0) ) ) )
    case "e"     ConvertValue = String.Format("%G",Math.CastLong2Float(Lang.MakeLong4( _
                                               String.SafeParse(gridObj.getVal(line,col),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+1),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+2),0) , _
                                               String.SafeParse(gridObj.getVal(line,col+3),0) ) ) )
    case else    ConvertValue = "Format undefined"
  End Select
End Function


