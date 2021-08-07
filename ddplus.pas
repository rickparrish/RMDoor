unit DDPlus;

{$mode objfpc}{$H-}

interface

const
  ProgName: string[60] = 'DDPlus via RMDoor';

var
  AnsiOn:       Boolean;                    {Process ANSI locally                    }
  Default_Fore: Byte;                       {default foreground color                }
  Graphics:     Byte;                       {from DROP FILE: graphics code           }
  Local:        Boolean;                    {from DROP FILE: local mode              }
  LocalCol:     Boolean;                    {From .CTL file: Local color enabled     }
  MaxTime:      Word;                       {from .CTL file: maximum time in door    }
  Node_Num:     Integer;                    {Node number                             }
  NoTime:       String;                     {Out of time filename                    }
  Stacked:      String;                     {used internally - stacked commands      }
  StatLine:     Boolean;                    {status line background                  }
  User_First_Name: String[30];              {from DROP FILE: user's first name       }
  User_Last_Name: String[30];               {from DROP FILE: user's last name        }

procedure Display_Status(AForce: Boolean);    {jakob added force!}
procedure DisplayFile1(const AFileName: string); {jakobs special! remove original!}
procedure Elapsed(AHour1, AMin1, ASec1, AHour2, AMin2, ASec2: LongInt; var AOutHour, AOutMin, AOutSec: LongInt);
procedure InitDoorDriver(AConfigFileName: String);
function LoCase(ACh: Char): Char;
procedure PromptJ(var AText: String; ALength: Integer; APromptColour: Boolean; AGetStack: Boolean);
procedure PromptSec(var AText: String; ALength: Integer; APromptColour: Boolean; AMask: Char); {jakobs own edited version}
procedure PromptWR(var AText: String; ALength: Integer; APromptColour: Boolean); {jakobs own edited version}
procedure ReleaseTimeSlice;
procedure Set_Background(AColour: Byte);
procedure Set_Color(AFore, ABack: Byte);
procedure Set_Foreground(AColour: Byte);
procedure SClrScr;
procedure SGoto_XY(AX, AY: Integer);
function SKeyPressed: Boolean;
procedure SRead_Char(var ACh: Char);
procedure SWrite(AText: String);
procedure SWriteC(ACh: Char);
procedure SWriteLn(AText: String);
function Time_Left: Integer;
function va(AInt: Integer): String;

implementation

uses
  Ansi, Door, FileUtils,
  SysUtils;

procedure Display_Status(AForce: Boolean);    {jakob added force!}
begin
  if Assigned(DoorOnStatusBar) AND NOT(DoorLocal) AND NOT(DoorSTDIO) then DoorOnStatusBar;
end;

procedure DisplayFile1(const AFileName: string); {jakobs special! remove original!}
var
  Ch: Char;
  F: Text;
  LineIndex: Integer;
  NonStop: Boolean;
  Quit: Boolean;
  S: String;
begin
  // TODOX Not exactly matching logic
  // If filename contains period, use it.
  // Otherwise try extension based on graphcs mode with fallback to lower graphics modes until a match is found
  // (ie ANSI tries ANSI then ASCII, RIP tries RIP then ANSI then ASCII)
  if NOT(OpenFileForRead(F, AFileName, 100)) then
  begin
    if NOT(OpenFileForRead(F, AFileName + '.ANS', 100)) then
    begin
      if NOT(OpenFileForRead(F, AFileName + '.ASC', 100)) then
      begin
        // Unable to read file:
        // d(error_col, error_mes + 'Unable to READ : ' + s);
        // d(error_col, '(' + error_message(error) + ')');
        DoorTextColour(12);
        DoorWriteLn('File-Alert! Unable to READ : ' + AFileName);
        Exit;
      end;
    end;
  end;

  LineIndex := 0;
  NonStop := false;
  Quit := false;
  repeat
    ReadLn(F, S);
    DoorWriteLn(S);

    // If a key was hit, read it.  If it was CTRL-C, CTRL-K, or S, stop output
    if (DoorKeyPressed) then
    begin
      Ch := DoorReadKey;
      if (Ch in [^C, ^K, 's', 'S']) then
      begin
        Quit := true;
      end;
    end;

    // If we've shown a screen full of text, then show the continue prompt
    if NOT(Quit) then
    begin
      Inc(LineIndex);
      if (LineIndex mod 23 = 0) AND NOT(NonStop) then // TODOX 23
      begin
        DoorWrite('(C)ontinue, (N)on-stop, (S)top ? ');
        Ch := DoorReadKey;
        DoorWrite(AnsiGotoX(1) + AnsiClrEol);
        if Ch in ['S', 's'] then
        begin
          Quit := true;
        end;
        if Ch in ['N', 'n'] then
        begin
          NonStop := true;
        end;
      end;
    end;
  until EOF(F) OR Quit;
  Close(F);

  DoorTextColour(Default_Fore);
end;

procedure Elapsed(AHour1, AMin1, ASec1, AHour2, AMin2, ASec2: LongInt; var AOutHour, AOutMin, AOutSec: LongInt);
var
  a, b, c: Integer;
begin
  if AHour1 < AHour2 then
    AHour1 := AHour1 + 24;
  a := (AHour1 * 3600) + (AMin1 * 60) + ASec1;
  b := (AHour2 * 3600) + (AMin2 * 60) + ASec2;
  c := a - b;
  AOutHour := c div 3600;
  c := c mod 3600;
  AOutMin := c div 60;
  c := c mod 60;
  AOutSec := c;
end;

procedure InitDoorDriver(AConfigFileName: String);
var
  I: Integer;
  SawSpace: Boolean;
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  DoorStartUp;

  if (DoorDropInfo.Emulation = etASCII) then
  begin
    Graphics := 2;
  end else
  begin
    Graphics := 3;
  end;

  Local := DoorLocal;
  MaxTime := DoorDropInfo.MaxSeconds div 60;
  Node_Num := DoorDropInfo.Node;

  User_First_Name := '';
  User_Last_Name := '';
  SawSpace := false;
  for I := 1 to Length(DoorDropInfo.RealName) do
  begin
    if (DoorDropInfo.RealName[I] = ' ') then
    begin
      SawSpace := true;
    end else
    if (SawSpace) then
    begin
      User_Last_Name := User_Last_Name + DoorDropInfo.RealName[I];
    end else
    begin
      User_First_Name := User_First_Name + DoorDropInfo.RealName[I];
    end;
  end;
end;

function LoCase(ACh: Char): Char;
begin
  Result := LowerCase(ACh);
end;

procedure PromptJ(var AText: String; ALength: Integer; APromptColour: Boolean; AGetStack: Boolean);
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  AText := DoorInput(AText, DOOR_INPUT_CHARS_ALL, #0, ALength, ALength, 23);
end;

procedure PromptSec(var AText: String; ALength: Integer; APromptColour: Boolean; AMask: Char); {jakobs own edited version}
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  AText := DoorInput(AText, DOOR_INPUT_CHARS_ALL, AMask, ALength, ALength, 23);
end;

procedure PromptWR(var AText: String; ALength: Integer; APromptColour: Boolean); {jakobs own edited version}
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  AText := DoorInput(AText, DOOR_INPUT_CHARS_ALL, #0, ALength, ALength, 23);
end;

procedure ReleaseTimeSlice;
begin
  // TODOX Confirm this is CPU friendly on all platforms
  Sleep(1);
end;

procedure SClrScr;
begin
  DoorClrScr;
end;

procedure Set_Background(AColour: Byte);
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  DoorTextBackground(AColour);
end;

procedure Set_Color(AFore, ABack: Byte);
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  DoorTextBackground(ABack);
  DoorTextColour(AFore);
end;

procedure Set_Foreground(AColour: Byte);
begin
  // TODOX Big function, haven't confirmed matching implementation yet
  DoorTextColour(AColour);
end;

procedure SGoto_XY(AX, AY: Integer);
begin
  DoorGotoXY(AX, AY);
end;

function SKeyPressed: Boolean;
begin
  // TODOX References macro variable, grep says it may not be used
  Result := DoorKeyPressed;
end;

procedure SRead_Char(var ACh: Char);
begin
  // TODOX Has some weird stuff for macros, but grep shows it likely isn't used
  ACh := DoorReadKey;
end;

procedure SWrite(AText: String);
begin
  DoorWrite(AText);
end;

procedure SWriteC(ACh: Char);
begin
  DoorWrite(ACh);
end;

procedure SWriteLn(AText: String);
begin
  DoorWriteLn(AText);

  (*TODOX
  Inc(curlinenum);
  if (curlinenum = (numlines - 1)) then
  begin;
    curlinenum := 1;
    if moreok then
      morecheck;
  end;*)
end;

function Time_Left: Integer;
begin
  Result := DoorSecondsLeft div 60;
end;

function va(AInt: Integer): String;
begin
  Result := IntToStr(AInt);
end;

begin
  // TODOX Most of these should be updated in InitDoorDriver
  AnsiOn := true;
  Default_Fore := 7;
  Graphics := 3; // ANSI
  Local := false;
  LocalCol := true;
  MaxTime := 60;
  Node_Num := 0;
  NoTime := 'Out of time!';
  Stacked := '';
  StatLine := true;
  User_First_Name := 'Usurper';
  User_Last_Name := 'Fan';
end.
