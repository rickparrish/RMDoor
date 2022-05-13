unit Door;

{$mode objfpc}{$h+}

interface

uses
  Ansi, Comm, DropFiles, StringUtils, VideoUtils,
  Classes, Crt, DateUtils, StrUtils, SysUtils;

const
  DOOR_INPUT_CHARS_ALL = '`1234567890-=\qwertyuiop[]asdfghjkl;''zxcvbnm,./~!@#$%^&*()_+|QWERTYUIOP{}ASDFGHJKL:"ZXCVBNM<>? ';
  DOOR_INPUT_CHARS_ALPHA = 'qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM';
  DOOR_INPUT_CHARS_NUMERIC = '1234567890';
  DOOR_INPUT_CHARS_FILENAME = '1234567890-=\qwertyuiop[]asdfghjkl;''zxcvbnm,.~!@#$%^&()_+QWERTYUIOP{}ASDFGHJKL:ZXCVBNM ';

type
  TDoorEmulationType = (etANSI, etASCII);

  {
    When a dropfile is read there is some useless information so it is not
    necessary to store the whole thing in memory.  Instead only certain
    parts are saved to this record

    Supported Dropfiles
    A = Found In DOOR32.SYS
    B = Found In DORINFO*.DEF
    C = Found In DOOR.SYS
    D = Found In INFO.*
    E = Supported By WINServer
    F = Found in Phenom Drop
  }
  TDoorDropInfo = Record
    Access    : LongInt;            {ABC--F} {User's Access Level}
    Alias     : String;             {ABCDEF} {User's Alias/Handle}
    XtendPalette: boolean;          {-----F} {Supports extended palette}
    LoadableFonts: boolean;         {-----F} {Supports loadable fonts}
    Baud      : LongInt;            {ABCDE-} {Connection Baud Rate}
    BBSName   : String;             {-----F} {Name of the BBS}
    Clean     : Boolean;            {---D--} {Is LORD In Clean Mode?}
    ComNum    : LongInt;            {ABCD--} {Comm/Socket Number}
    ComType   : Byte;               {A-----} {Comm Type (0=Local, 1=Serial, 2=Socket, 3=WC5}
    Columns   : LongInt;            {-----F} {Columns}
    Domain    : String;             {-----F} {BBS Domain}
    Directory : String;             {-----F} {BBS Directory}
    Emulation : TDoorEmulationType; {ABCDEF} {User's Emulation (etANSI or etASCII)}
    Fairy     : Boolean;            {---D--} {Does LORD User Have Fairy?}
    MaxSeconds: LongInt;            {ABCDEF} {User's Time Left At Start (In Seconds)}
    Node      : LongInt;            {A-C-EF} {Node Number}
    OSType    : String;             {-----F} {OS Type} 
    RealName  : String;             {ABCDE-} {User's Real Name}
    RecPos    : LongInt;            {A-CD--} {User's Userfile Record Position (Always 0 Based)}
    Registered: Boolean;            {---D--} {Is LORD Registered?}
    Rows      : LongInt;            {-----F} {Rows}
    Sysop     : String;             {-----F} {Sysop Name}
    SecurityLevel: LongInt;         {-----F} {Security Level}
  end;

  TDoorLastKeyType = (lkNone, lkLocal, lkRemote);

  {
    Information about the last key pressed is stored in this record.
    This should be considered read-only.
  }
  TDoorLastKey = Record
    Ch: Char;                   { Character of last key }
    Extended: Boolean;          { Was character preceded by #0 }
    Location: TDoorLastKeyType; { Location of last key }
    Time: TDateTime;            { SecToday of last key }
  end;

  {
    MORE prompts will use these two lines based on whether use has ANSI or ASCII
  }
  TDoorMOREPrompts = Record
    ASCII: String;           { Used by people with ASCII }
    ANSI: String;            { Used by people with ANSI }
    ANSITextLength: Integer; { ANSI may have non-displaying characters, we need to know the length of just the text }
  end;

  {
    Information about the current session is stored in this record.
  }
  TDoorSession = Record
    DoIdleCheck: Boolean;  { Check for idle timeout? }
    Events: Boolean;       { Run Events in mKeyPressed function }
    EventsTime: TDateTime; { MSecToday of last Events run }
    MaxIdle: LongInt;      { Max idle before kick (in seconds) }
    SethWrite: Boolean;    { Whether to interpret ` codes }
    TimeOn: TDateTime;     { SecToday program was started }
  end;

var
  DoorDropInfo: TDoorDropInfo;
  DoorLastKey: TDoorLastKey;
  DoorLiteBarIndex: Integer;
  DoorLiteBarOptions: TStringList;
  DoorMOREPrompts: TDoorMOREPrompts;
  DoorProgramNameAndVersion: String;
  DoorSession: TDoorSession;

  {
    Event variables that may be called at various times throughout
    the programs execution.  Assign them to your own procedures to
    give your program a more unique look
  }
  DoorOnCLP: Procedure(AKey: Char; AValue: String);
  DoorOnHangup: Procedure;
  DoorOnLocalLogin: Procedure;
  DoorOnStatusBar: Procedure;
  DoorOnSysopKey: Function(AKey: Char): Boolean;
  DoorOnTimeOut: Procedure;
  DoorOnTimeOutWarning: Procedure(AMinutes: Byte);
  DoorOnTimeUp: Procedure;
  DoorOnTimeUpWarning: Procedure(AMinutes: Byte);
  DoorOnUsage: Procedure;

function DoorCarrier: Boolean;
procedure DoorClose(ADisconnect: Boolean);
procedure DoorClrScr;
procedure DoorCursorDown(ACount: Byte);
procedure DoorCursorLeft(ACount: Byte);
procedure DoorCursorRestore;
procedure DoorCursorRight(ACount: Byte);
procedure DoorCursorSave;
procedure DoorCursorUp(ACount: Byte);
procedure DoorDisplayFile(AFilename: String);
procedure DoorGotoX(AX: Byte);
procedure DoorGotoXY(AX, AY: Byte);
procedure DoorGotoY(AY: Byte);
function DoorInput(ADefaultText, AAllowedCharacters: String; APasswordCharacter: Char; AVisibleLength, AMaxLength, AAttr: Byte): String;
function DoorKeyPressed: Boolean;
function DoorLiteBar(APageSize: Integer): Boolean;
function DoorLocal: Boolean;
function DoorOpenComm: Boolean;
function DoorReadKey: Char;
function DoorSecondsIdle: LongInt;
function DoorSecondsLeft: LongInt;
procedure DoorShutDown;
procedure DoorStartUp;
function DoorSTDIO: Boolean;
procedure DoorTextAttr(AAttr: Byte);
procedure DoorTextBackground(AColour: Byte);
procedure DoorTextColour(AColour: Byte);
procedure DoorTextColourAndBlink(AColour: Byte; ABlink: Boolean);
procedure DoorWrite(AText: String);
procedure DoorWriteCentered(AText: String);
procedure DoorWriteLn;
procedure DoorWriteLn(AText: String);

implementation

var
  OldExitProc: Pointer;
  STDIO: Boolean;

procedure DoorDoEvents; forward;
procedure NewExitProc; forward;

{
  Default action to take when user drops carrier
}
procedure DefaultOnHangup;
begin
  TextAttr := 15;
  ClrScr;
  WriteLn;
  WriteLn('   Caller Dropped Carrier.  Returning To BBS...');
  Delay(2500);
  Halt;
end;

{
  Default action to take when /L is used on the command-line
}
procedure DefaultOnLocalLogin;
var
  S: String;
begin
  DoorTextAttr(7);
  DoorClrScr;
  DoorWriteLn;
  DoorWriteLn;
  DoorWriteLn('  |1F  LOCAL LOGIN - FOR NODE ' + IntToStr(DoorDropInfo.Node) + '|07');
  DoorWriteLn;
  DoorWriteLn;
  DoorWriteLn('  |09Run ' + ExtractFileName(ParamStr(0)) + ' /? for command-line usage help|07');
  DoorWriteLn;
  repeat
    DoorWrite('  |0AName or handle |0F:|07 ');
    S := DoorInput('SYSOP', DOOR_INPUT_CHARS_ALPHA + ' ', #0, 40, 40, 31);
  until (S <> '');
  DoorDropInfo.RealName := S;
  DoorDropInfo.Alias := S;
end;

{
  Default status bar displays
}
procedure DefaultOnStatusBar;
begin
  FastWrite(#254 + '                           ' + #254 + '                   ' + #254 + '             ' + #254 + '                ' + #254, 1, 25, 30);
  FastWrite(PadRight(DoorDropInfo.RealName, 22), 3, 25, 31);
  FastWrite(DoorProgramNameAndVersion, 31, 25, 31);
  FastWrite(PadRight('Idle: ' + SecToMS(SecondsBetween(Now, DoorLastKey.Time)), 11), 51, 25, 31);
  FastWrite('Left: ' + SecToHMS(DoorSecondsLeft), 65, 25, 31);
end;

{
  Default action to take when the user idles too long
}
procedure DefaultOnTimeOut;
begin
  DoorTextAttr(15);
  DoorClrScr;
  DoorWriteLn;
  DoorWriteLn('  Idle Time Limit Exceeded.  Returning To BBS...');
  Delay(2500);
  Halt;
end;

{
  Default action to take when the user runs out of time
}
procedure DefaultOnTimeUp;
begin
  DoorTextAttr(15);
  DoorClrScr;
  DoorWriteLn;
  DoorWriteLn('  Your Time Has Expired.  Returning To BBS...');
  Delay(2500);
  Halt;
end;

{
  Default command-line help screen
}
procedure DefaultOnUsage;
begin
  // TODO Only display dropfiles that the current platform supports
  ClrScr;
  WriteLn;
  WriteLn(' USAGE: ' + ExtractFileName(ParamStr(0)) + ' <PARAMETERS>');
  WriteLn;
  WriteLn(' Load settings from a dropfile (DOOR32.SYS, DOOR.SYS, DORINFO*.DEF or INFO.*)');
  WriteLn('  -D         PATH\FILENAME OF DROPFILE');
  WriteLn(' Example: ' + ExtractFileName(ParamStr(0)) + ' -DC:\BBS\NODE1\DOOR32.SYS');
  WriteLn;
  WriteLn(' Pass settings on command-line');
  WriteLn('  -N         NODE NUMBER');
  WriteLn('  -S         SOCKET HANDLE');
  WriteLn(' Example: ' + ExtractFileName(ParamStr(0)) + ' -N1 -S1000');
  WriteLn;
  WriteLn(' Run in local mode');
  WriteLn('  -L         LOCAL MODE');
  WriteLn(' Example: ' + ExtractFileName(ParamStr(0)) + ' -L');
  WriteLn;
  WriteLn(' Optional parameters');
  //TODO WriteLn('  -W         WINSERVER DOOR32 MODE');
  WriteLn('  -X         DISABLE COMM ROUTINES (STDIO MODE)');
  WriteLn;
  Halt;
end;

{
  Returns TRUE unless the user has dropped carrier
}
function DoorCarrier: Boolean;
begin
  Result := DoorLocal OR STDIO OR CommCarrier;
end;

procedure DoorClose(ADisconnect: Boolean);
begin
  if Not(DoorLocal) AND NOT(STDIO) then CommClose(ADisconnect);
end;

{
  Clears the entire screen and puts the cursor at (1, 1)
}
procedure DoorClrScr;
begin
  DoorWrite(AnsiClrScr);
end;

{
  Move the cursor down ACOUNT lines without changing the X position
}
procedure DoorCursorDown(ACount: Byte);
begin
  DoorWrite(AnsiCursorDown(ACount));
end;

{
  Move the cursor left ACOUNT columns without changing the Y position
}
procedure DoorCursorLeft(ACount: Byte);
begin
  DoorWrite(AnsiCursorLeft(ACount));
end;

{
  Restore the cursor position which was previously saved with mCursorSave
}
procedure DoorCursorRestore;
begin
  DoorWrite(AnsiCursorRestore);
end;

{
  Move the cursor right ACOUNT columns without changing the Y position
}
procedure DoorCursorRight(ACount: Byte);
begin
  DoorWrite(AnsiCursorRight(ACount));
end;

{
  Move the cursor up ACOUNT lines without changing the X position
}
procedure DoorCursorUp(ACount: Byte);
begin
  DoorWrite(AnsiCursorUp(ACount));
end;

{
  Save the current cursor position.  Restore it later with mCursorRestore
}
procedure DoorCursorSave;
begin
  DoorWrite(AnsiCursorSave);
end;

{
  DoorKeyPressed calls this procedure every time it is run.  This is where
  a lot of the "behind the scenes" stuff happens, such as determining how
  much time the user has left, if theyve dropped carrier, and updating the
  status bar.
  It is not recommended that you mess with anything in this procedure
}
procedure DoorDoEvents;
begin
  if (DoorSession.Events) and (SecondsBetween(Now, DoorSession.EventsTime) >= 1) then
  begin
    {Check For Hangup}
    if Not(DoorCarrier) and Assigned(DoorOnHangup) then DoorOnHangup;

    {Check For Idle Timeout}
    if (DoorSession.DoIdleCheck) and (DoorSecondsIdle > DoorSession.MaxIdle) and Assigned(DoorOnTimeOut) then DoorOnTimeOut;

    {Check For Idle Timeout Warning}
    if (DoorSession.DoIdleCheck) and ((DoorSession.MaxIdle - DoorSecondsIdle) mod 60 = 1) and ((DoorSession.MaxIdle - DoorSecondsIdle) div 60 <= 5) and (Assigned(DoorOnTimeOutWarning)) then DoorOnTimeOutWarning((DoorSession.MaxIdle - DoorSecondsIdle) div 60);

    {Check For Time Up}
    if (DoorSecondsLeft < 1) and Assigned(DoorOnTimeUp) then DoorOnTimeUp;

    {Check For Time Up Warning}
    if (DoorSecondsLeft mod 60 = 1) and (DoorSecondsLeft div 60 <= 5) and Assigned(DoorOnTimeUpWarning) then DoorOnTimeUpWarning(DoorSecondsLeft div 60);

    {Update Status Bar}
    if Assigned(DoorOnStatusBar) AND NOT(DoorLocal) AND NOT(STDIO) then DoorOnStatusBar;

    DoorSession.EventsTime := Now;
  end;
end;

{
  Display a file to screen
}
procedure DoorDisplayFile(AFilename: String);
var
  InFile: TextFile;
  S: String;
begin
  // Set the name of the file that will be read
  AssignFile(InFile, AFilename);

  // Embed the file handling in a try/except block to handle errors gracefully
  try
    // Open the file for reading
    Reset(InFile);

    // Keep reading lines until the end of the file is reached
    while not Eof(InFile) do
    begin
      ReadLn(InFile, S);
      DoorWrite(S);
      if Not(Eof(InFile)) then DoorWriteLn;
    end;

    // Done so close the file
    CloseFile(InFile);
  except
    on E: EInOutError do
    begin
      DoorWriteLn('Error reading "' + AFilename + '": ' + E.Message);
      DoorWriteLn('Hit a key to continue');
      DoorReadKey;
    end;
  end;
end;

{
  Go to column AX on the current line
}
procedure DoorGotoX(AX: Byte);
begin
  DoorWrite(AnsiGotoX(AX));
end;

{
  Go to column AX and line AY on the current screen
}
procedure DoorGotoXY(AX, AY: Byte);
begin
  DoorWrite(AnsiGotoXY(AX, AY));
end;

{
  Go to line AY on the current column
}
procedure DoorGotoY(AY: Byte);
begin
  DoorWrite(AnsiGotoY(AY));
end;

{
  A fancy input routine

  ADefaultText         - The text initially displayed in the edit box
  AAllowedCharacters   - The characters ALLOWED to be part of the string
                         Look in MSTRINGS.PAS for some defaults
  APasswordCharacter   - The password character shown instead of the actual text
                         Use #0 if you dont want to hide the text
  AVisibleLength       - The number of characters big the edit box should be on screen
  AMaxLength           - The number of characters the edit box should allow
                         AMaxLen can be larger than AShowLen, it will just scroll
                         if that happens.
  AAttr                - The text attribute of the editbox's text and background
                         Use formula Attr = Foreground + (Background * 16)

  If the user pressed ESCAPE then ADefaultText is returned.  If they hit enter
  the current string is returned.  They cannot hit enter on a blank line.
}
function DoorInput(ADefaultText, AAllowedCharacters: String; APasswordCharacter: Char; AVisibleLength, AMaxLength, AAttr: Byte): String;
var
   Ch: Char;
   S: String;
   SavedAttr: Byte;
   XPos: Byte;

  procedure UpdateText;
  begin
    DoorGotoX(XPos);
    if (Length(S) > AVisibleLength) then
    begin
      if (APasswordCharacter = #0) then
      begin
       DoorWrite(Copy(S, Length(S) - AVisibleLength + 1, AVisibleLength))
      end else
      begin
        DoorWrite(AddCharR(APasswordCharacter, '', AVisibleLength));
      end;
      DoorGotoX(XPos + AVisibleLength);
    end else
    begin
      if (APasswordCharacter = #0) then
      begin
       DoorWrite(S)
      end else
      begin
        DoorWrite(AddCharR(APasswordCharacter, '', Length(S)));
      end;
      DoorWrite(PadRight('', AVisibleLength - Length(S)));
      DoorGotoX(XPos + Length(S));
    end;
  end;

begin
  if (Length(ADefaultText) > AMaxLength) then ADefaultText := Copy(ADefaultText, 1, AMaxLength);
  S := ADefaultText;

  SavedAttr := TextAttr;
  DoorTextAttr(AAttr);
  XPos := WhereX;

  UpdateText;

  repeat
    Ch := DoorReadKey;
    if (Ch = #8) and (Length(S) > 0) then
    begin
     Delete(S, Length(S), 1);
     DoorWrite(#8 + ' ' + #8);
     if (Length(S) >= AVisibleLength) then UpdateText;
    end else
    if (Ch = #25) and (S <> '') then {CTRL-Y}
    begin
     S := '';
     UpdateText;
    end else
    if (Pos(Ch, AAllowedCharacters) > 0) and (Length(S) < AMaxLength) then
    begin
      S := S + Ch;
      if (Length(S) > AVisibleLength) then
      begin
         UpdateText
      end else
      if (APasswordCharacter = #0) then
      begin
         DoorWrite(Ch)
      end else
      begin
          DoorWrite(APasswordCharacter);
      end;
    end;
  until (Ch = #27) or (Ch = #13);

  DoorTextAttr(SavedAttr);
  DoorWriteLn;

  if (Ch = #27) then S := ADefaultText;
  Result := S;
end;

{
  Returns TRUE if a character is waiting to be read
  Also calls DoEvents to make sure the "dirty work" is handled
}
function DoorKeyPressed: Boolean;
begin
  DoorDoEvents;
  Result := KeyPressed;
  if Not(DoorLocal) AND NOT(STDIO) then Result := Result OR CommCharAvail;
end;

function DoorLiteBar(APageSize: Integer): Boolean;
var
  Ch: Char;
  I: Integer;
begin
  // Assume success
  Result := True;

  // Output options
  DoorTextAttr(15);
  DoorCursorSave;
  for I := 0 to DoorLiteBarOptions.Count - 1 do
  begin
    // Only output as many items as requested
    if (I = APageSize) then Break;

    if (I > 0) then
    begin
      DoorCursorRestore;
      DoorCursorDown(I);
    end;

    if (I = DoorLiteBarIndex) then DoorTextBackground(Crt.Blue);
    DoorWrite(DoorLiteBarOptions[I]);
    DoorTextAttr(15);
  end;

  // Get response
  repeat
      Ch := UpCase(DoorReadKey);
      case Ch of
          '8', '4', 'H', 'K':
          begin
            // TODO Once scrolling to next page is enabled, allow scrolling to previous page
            if (DoorLiteBarIndex > 0) then
            begin
              // Erase old highlight
              DoorCursorRestore;
              if (DoorLiteBarIndex > 0) then DoorCursorDown(DoorLiteBarIndex);
              DoorWrite(DoorLiteBarOptions[DoorLiteBarIndex]);
              DoorTextAttr(15);

              // Move up
              DoorLiteBarIndex -= 1;

              // Draw new highlight
              DoorCursorRestore;
              if (DoorLiteBarIndex > 0) then DoorCursorDown(DoorLiteBarIndex);
              DoorTextBackground(Crt.BLUE);
              DoorWrite(DoorLiteBarOptions[DoorLiteBarIndex]);
              DoorTextAttr(15);
            end;
          end;
          '6', '2', 'M', 'P':
          begin
            // TODO Allow scrolling to the next page
            if ((DoorLiteBarIndex < (APageSize - 1)) AND (DoorLiteBarIndex < (DoorLiteBarOptions.Count - 1))) then
            begin
              // Erase old highlight
              DoorCursorRestore;
              if (DoorLiteBarIndex > 0) then DoorCursorDown(DoorLiteBarIndex);
              DoorWrite(DoorLiteBarOptions[DoorLiteBarIndex]);
              DoorTextAttr(15);

              // Move up
              DoorLiteBarIndex += 1;

              // Draw new highlight
              DoorCursorRestore;
              if (DoorLiteBarIndex > 0) then DoorCursorDown(DoorLiteBarIndex);
              DoorTextBackground(Crt.BLUE);
              DoorWrite(DoorLiteBarOptions[DoorLiteBarIndex]);
              DoorTextAttr(15);
            end;
          end;

          'Q':
          begin
            Result := False;
            Break;
          end;
      end;
  until (Ch = #13);

  DoorCursorRestore;
end;

{
  Returns TRUE if the door is being run in local mode
}
function DoorLocal: Boolean;
begin
  Result := DoorDropInfo.ComType = 0;
end;

{
  Returns TRUE if it was able to open an existing connection
  mStartUp calls this, so you should never have to directly
}
function DoorOpenComm: Boolean;
{$IFDEF WIN32_TODO_WINSERVER}
var
  WC5User: TWC5User;
{$ENDIF}
begin
  if (DoorDropInfo.ComNum = 0) or (DoorDropInfo.ComType = 0) OR (STDIO) then
  begin
   DoorOpenComm := true;
  end else
  begin
    CommOpen(DoorDropInfo.ComNum);
    DoorOpenComm := CommCarrier;

    {$IFDEF WIN32_TODO_WINSERVER}
    if (DropInfo.ComType = 3) then
    begin
         if (InitWC5) then
         begin
              if (WC5_WildcatLoggedIn^(WC5User) = 1) then
              begin
                   DropInfo.RealName := WC5User.Info.Name;
                   DropInfo.Alias := GetFName(WC5User.Info.Name);
                   DropInfo.MaxTime := WC5User.TimeLeftToday * 60;
                   if (WC5User.TerminalType = 0) then
                      DropInfo.Emulation := etANSI
                   else
                       DropInfo.Emulation := etASCII;
                   DropInfo.Node := WC5_GetNode^();
              end else
                  mOpen := False;
         end else
             mOpen := False;
    end;
    {$ENDIF}
  end;
end;

{
  Returns the next character in the input buffer and updates the TLastKey
  record.
}
function DoorReadKey: Char;
var
  Ch: Char;
  I: Integer;
begin
  Ch := #0;
  DoorLastKey.Location := lkNone;
  repeat
    while Not(DoorKeyPressed) do Sleep(1);

    if (KeyPressed) then
    begin
      // Check for local keypress
      Ch := ReadKey;
      if (Ch = #0) then
      begin
        Ch := ReadKey;
        if (Not(Assigned(DoorOnSysopKey)) OR (Not(DoorOnSysopKey(Ch)))) then
        begin
          DoorLastKey.Extended := True;
          DoorLastKey.Location := lkLocal;
        end;
      end else
      begin
        DoorLastKey.Extended := False;
        DoorLastKey.Location := lkLocal;
      end;
    end else
    if Not(DoorLocal) AND NOT(STDIO) AND (CommCharAvail) then
    begin
      // Check for remote keypress
      Ch := CommReadChar;
      if (Ch = #27) then
      begin
        // ESC, could be a special key

        // Wait up to 500ms for a second key
        for I := 1 to 10 do
        begin
          if Not(CommCharAvail) then Sleep(50);
        end;

        // Read the next key, if we have one
        if (CommCharAvail) then
        begin
          if (CommPeekChar = '[') then
          begin
            // That's ESC and [ now, so we'll assume it's a special key
            CommReadChar; // Eat the [

            // Wait up to 500ms for a second key
            for I := 1 to 10 do
            begin
              if Not(CommCharAvail) then Sleep(50);
            end;

            if (CommCharAvail) then
            begin
              Ch := CommReadChar;
              case Ch of
                'A': begin
                       Ch := 'H'; // Up arrow
                       DoorLastKey.Extended := True;
                       DoorLastKey.Location := lkRemote;
                     end;
                'B': begin
                       Ch := 'P'; // Down arrow
                       DoorLastKey.Extended := True;
                       DoorLastKey.Location := lkRemote;
                     end;
                'C': begin
                       Ch := 'M'; // Right arrow
                       DoorLastKey.Extended := True;
                       DoorLastKey.Location := lkRemote;
                     end;
                'D': begin
                       Ch := 'K'; // Left arrow
                       DoorLastKey.Extended := True;
                       DoorLastKey.Location := lkRemote;
                     end;
              end;
            end else
            begin
              // ESC and [ with no other key, weird combo to hit manually so we'll ignore it
            end;
          end else
          begin
            // Looks like it was ESC followed by something else
            DoorLastKey.Extended := False;
            DoorLastKey.Location := lkRemote;
          end;
        end else
        begin
          // No next key, guess it was just an escape keypress
          DoorLastKey.Extended := False;
          DoorLastKey.Location := lkRemote;
        end;
      end else
      begin
        DoorLastKey.Extended := False;
        DoorLastKey.Location := lkRemote;
      end;
    end;
  until (DoorLastKey.Location <> lkNone);

  DoorLastKey.Ch := Ch;
  DoorLastKey.Time := Now;

  Result := Ch;
end;

{
  Returns the number of seconds the user has been idle
}
function DoorSecondsIdle: LongInt;
begin
     Result := SecondsBetween(Now, DoorLastKey.Time);
end;

{
  Returns the number of seconds the user has left this session
}
function DoorSecondsLeft: LongInt;
begin
     Result := DoorDropInfo.MaxSeconds - SecondsBetween(Now, DoorSession.TimeOn);
end;

procedure DoorShutDown;
begin
  DoorClose(false);
  DoorLiteBarOptions.Free;
end;

{
  This is the first call your door should make before making any other call
  to procedures in this unit.  It will parse the command line and take
  action depending on the parameters it receives.
  If a dropfile is to be read, it will happen automatically.
  If the program is not being run in local mode, the existing connection
  to the remote user will be opened.
}
procedure DoorStartUp;
var
   Ch: Char;
   DropFile: String;
   Socket: LongInt;
   I: Integer;
   Local: Boolean;
   Node: Integer;
   S: String;
   Wildcat: Boolean;
begin
  DropFile := '';
  Local := True;
  Node := 0;
  Socket := -1;
  STDIO := False;
  Wildcat := False;

  if (ParamCount > 0) then
  begin
    for I := 1 to ParamCount do
    begin
      S := ParamStr(I);
      if (Length(S) >= 2) and (S[1] in ['/', '-']) then
      begin
        Ch := UpCase(S[2]);
        Delete(S, 1, 2);
        Case UpCase(Ch) of
          '?': Local := False;
          'D': begin
                 Local := False;
                 DropFile := S;
               end;
          'N': Node := StrToIntDef(S, 0);
          'S': begin
                 Local := False;
                 Socket := StrToIntDef(S, -1);
               end;
          {$IFDEF WIN32}
          'W': begin
                 Local := False;
                 Wildcat := True;
               end;
          {$ENDIF}
          'X': STDIO := True;
          else if Assigned(DoorOnCLP) then DoorOnCLP(Ch, S);
        end;
      end;
    end;
  end;

  if (Local) then
  begin
    DoorDropInfo.Node := Node;
    if Assigned(DoorOnLocalLogin) then
    begin
      DoorOnLocalLogin;
      DoorClrScr;
    end;
  end else
  if (Wildcat) then
  begin
    DoorDropInfo.ComNum := 1;
    DoorDropInfo.ComType := 3;
  end else
  if (Socket >= 0) and (Node > 0) then
  begin
    DoorDropInfo.ComNum := Socket;
    DoorDropInfo.ComType := 2;
    DoorDropInfo.Node := Node;
  end else
  if (DropFile <> '') then
  begin
    if (FileExists(DropFile)) and (AnsiContainsText(DropFile, 'DOOR32.SYS')) then
    begin
      ReadDoor32(DropFile);
    end else
    if (FileExists(DropFile)) and (AnsiContainsText(DropFile, 'DOOR.SYS')) then
    begin
      ReadDoorSys(DropFile);
    end else
    if (FileExists(DropFile)) and (AnsiContainsText(DropFile, 'DORINFO')) then
    begin
      ReadDorinfo(DropFile);
    end else
    if (FileExists(DropFile)) and (AnsiContainsText(DropFile, 'INFO.')) then
    begin
      ReadLordInfo(DropFile);
    end else
    if (FileExists(DropFile)) and (AnsiContainsText(DropFile, 'PHENOMDROP.TXT')) then
    begin
      ReadPhenomDrop(DropFile);
    end else
    begin
      ClrScr;
      WriteLn;
      WriteLn('  Drop File Does Not Exist Or Is Not Supported:');
      WriteLn('  ' + DropFile);
      WriteLn;
      Delay(2500);
      Halt;
    end;
  end else
  begin
    if Assigned(DoorOnUsage) then DoorOnUsage;
    Halt;
  end;

  DoorLastKey.Time := Now;
  DoorSession.TimeOn := Now;

  if Not(DoorLocal) then
  begin
    if Not(DoorOpenComm) then
    begin
      ClrScr;
      WriteLn;
      WriteLn('  No Carrier Detected');
      WriteLn;
      Delay(2500);
      Halt;
    end;

    // Setup exit proc, which ensures the comm is closed properly
    OldExitProc := ExitProc;
    ExitProc := @NewExitProc;

    // Indicate we want events to happen (timeout and timeup check, status bar, etc)
    DoorSession.Events := True;
    DoorSession.EventsTime := 0;

    DoorClrScr;
    Window(1, 1, 80, 24);
  end;
end;

{
  Returns TRUE if the door is being run in STDIO mode
}
function DoorSTDIO: Boolean;
begin
  Result := STDIO;
end;

{
  Change the current text foreground and background to AAttr
  Formula: Attr = Foreground + (Background * 16)
}
procedure DoorTextAttr(AAttr: Byte);
begin
  DoorWrite(AnsiTextAttr(AAttr));
end;

{
  Change the current text background to AColour
}
procedure DoorTextBackground(AColour: Byte);
begin
  DoorWrite(AnsiTextBackground(AColour));
end;

{
  Change the current text colour to AColour
}
procedure DoorTextColour(AColour: Byte);
begin
  DoorWrite(AnsiTextColour(AColour));
end;

{
  Change the current text colour to AColour and turn blink on/off
}
procedure DoorTextColourAndBlink(AColour: Byte; ABlink: Boolean);
begin
  DoorWrite(AnsiTextColour(AColour));
  DoorWrite(AnsiBlink(ABlink));
end;

{
  Writes a line of text to the screen
}
procedure DoorWrite(AText: String);
var
  BackTick2: String;
  BackTick3: String;
  BeforeBackTick: String;
begin
  if (Pos('|', AText) > 0) then AText := PipeToAnsi(AText);

  if (DoorSession.SethWrite AND (Pos('`', AText) > 0) AND (Length(AText) > 1)) then
  begin
    while (Length(AText) > 0) do
    begin
      // Write everything up to the next backtick
      if (Pos('`', AText) <> 1) then
      begin
        // First check if we have another backtick
        if (Pos('`', AText) = 0) then
        begin
          // Nope, so write everything and we're done
          DoorWrite(AText);
          AText := '';
        end else
        begin
          // Yep, so only write up until the backtick
          BeforeBackTick := Copy(AText, 1, Pos('`', AText) - 1);
          DoorWrite(BeforeBackTick);
          Delete(AText, 1, Length(BeforeBackTick));
        end;
      end;

      // Now we have a backtick at the beginning of the string
      while (Pos('`', AText) = 1) do
      begin
        if (Length(AText) = 1) then
        begin
          // Only single `, so clear the string
          AText := '';
        end else
        begin
          // At least 2 characters, so run with it
          BackTick2 := Copy(AText, 1, 2);

          case BackTick2 of
            '`1': DoorTextColourAndBlink(Blue, false);
            '`2': DoorTextColourAndBlink(Green, false);
            '`3': DoorTextColourAndBlink(Cyan, false);
            '`4': DoorTextColourAndBlink(Red, false);
            '`5': DoorTextColourAndBlink(Magenta, false);
            '`6': DoorTextColourAndBlink(Brown, false);
            '`7': DoorTextColourAndBlink(LightGray, false);
            '`8': DoorTextColourAndBlink(DarkGray, false);
            '`9': DoorTextColourAndBlink(LightBlue, false);
            '`0': DoorTextColourAndBlink(LightGreen, false);
            '`!': DoorTextColourAndBlink(LightCyan, false);
            '`@': DoorTextColourAndBlink(LightRed, false);
            '`#': DoorTextColourAndBlink(LightMagenta, false);
            '`$': DoorTextColourAndBlink(Yellow, false);
            '`%': DoorTextColourAndBlink(White, false);
            '`*': DoorTextColourAndBlink(Black, false);
            '`b': DoorTextColourAndBlink(Red, true);
            '`c': begin
                    DoorTextAttr(7);
                    DoorClrScr;
                    DoorWrite(#13#10#13#10);
                end;
            '`d': DoorWrite(#8);
            '`k': begin
                    DoorWrite('`r0  `2<`0MORE`2>');
                    DoorReadKey;
                    DoorWrite(#8#8#8#8#8#8#8#8 + '        ' + #8#8#8#8#8#8#8#8);
                end;
            '`l': Delay(500);
            '`w': Delay(100);
            '`x': DoorWrite(' ');
            '`y': DoorTextColourAndBlink(Yellow, true);
            '`\': DoorWrite(#13#10);
            '`|': DoorWrite('|`w`d\`w`d-`w`d/`w`d|`w`d\`w`d-`w`d/`w`d|`w`d `d');
            else
            begin
              if (Length(AText) >= 3) then
              begin
                BackTick3 := Copy(AText, 1, 3);
                if (Pos('`r', BackTick3) = 1) then
                begin
                  case BackTick3 of
                    '`r0': DoorTextBackground(Black);
                    '`r1': DoorTextBackground(Blue);
                    '`r2': DoorTextBackground(Green);
                    '`r3': DoorTextBackground(Cyan);
                    '`r4': DoorTextBackground(Red);
                    '`r5': DoorTextBackground(Magenta);
                    '`r6': DoorTextBackground(Brown);
                    '`r7': DoorTextBackground(LightGray);
                  end;

                  // Delete 1 char from beginning of string since `r is a sequence with 3 characters (and 2 more get deleted below)
                  Delete(AText, 1, 1);
                end;
              end;
            end;
          end;

          // Delete 2 characters to remove the handled sequence from the string
          Delete(AText, 1, 2);
        end;
      end;
    end;
  end else
  begin
    AnsiWrite(AText);
    if Not(DoorLocal) AND NOT(STDIO) then CommWrite(AText);
  end;
end;

{
  Writes a string centered on the screen.
}
procedure DoorWriteCentered(AText: String);
begin
  if (DoorSession.SethWrite) then
  begin
    DoorGotoX((80 - Length(SethStrip(AText))) div 2);
  end else
  begin
    DoorGotoX((80 - Length(AText)) div 2);
  end;

  DoorWrite(AText);
end;

{
  Writes a CR/LF
}
procedure DoorWriteLn;
begin
  DoorWrite(#13#10);
end;

{
  Writes a line of text to the screen, folled by a CR/LF
}
procedure DoorWriteLn(AText: String);
begin
  DoorWrite(AText + #13#10);
end;

{
  Custom exit proc to ensure mShutdown is called
}
procedure NewExitProc;
begin
     ExitProc := OldExitProc;

     DoorTextAttr(7);
     DoorCursorDown(255);
     DoorCursorLeft(255);

     if Not(DoorLocal) AND NOT(STDIO) then DoorClose(false);
end;

begin
  DoorOnCLP := nil;
  DoorOnHangup := @DefaultOnHangup;
  DoorOnLocalLogin := @DefaultOnLocalLogin;
  DoorOnStatusBar := @DefaultOnStatusBar;
  DoorOnSysopKey := nil;
  DoorOnTimeOut := @DefaultOnTimeOut;
  DoorOnTimeOutWarning := nil;
  DoorOnTimeUp := @DefaultOnTimeUp;
  DoorOnTimeUpWarning := nil;
  DoorOnUsage := @DefaultOnUsage;

  with DoorDropInfo do
  begin
       Access := 0;
       Alias := '';
       Clean := False;
       ComNum := 0;
       ComType := 0;
       Emulation := etANSI;
       Fairy := False;
       MaxSeconds := 3600;
       Node := 0;
       RealName := '';
       RecPos := 0;
       Registered := False;
  end;

  with DoorLastKey do
  begin
       Ch := #0;
       Extended := False;
       Location := lkNone;
       Time := 0;
  end;

  DoorLiteBarIndex := 0;
  DoorLiteBarOptions := TStringList.Create;

  with DoorMOREPrompts do
  begin
    ASCII := ' <MORE>';
    ANSI := ' |0A<|02MORE|0A>';
    ANSITextLength := 7;
  end;

  DoorProgramNameAndVersion := 'RMDoor v13.09.02';

  with DoorSession do
  begin
       DoIdleCheck := True;
       Events := False;
       EventsTime := 0;
       MaxIdle := 300;
       SethWrite := false;
       TimeOn := 0;
  end;
end.

