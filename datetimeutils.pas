unit DateTimeUtils;

{$mode objfpc}{$H+}

interface

var
  DaysBeforeMonth: Array[1..12] of Integer;
  DaysInMonth: Array[1..12] of Integer;

function DaysSince1Jan0001: LongInt; // Lifted from SWAGDATE.PAS
function IsLeapYear(AYear: Integer): Boolean; // Lifted from SWAGDATE.PAS

implementation

uses
  Dos;

function DaysSince1Jan0001: LongInt;
var
  YY, MM, DD, DOW: Word;
begin
  GetDate(YY, MM, DD, DOW);

  Result := DD;
  Inc(Result, DaysBeforeMonth[MM]);
  Inc(Result, (YY - 1) * 365);
  Inc(Result, ((YY - 1) div 4) - ((YY - 1) div 100) + ((YY - 1) div 400));
  if (MM > 2) AND IsLeapYear(YY) then Inc(Result, 1);
end;

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := ((AYear MOD 4 = 0) AND (AYear MOD 100 <> 0)) OR (AYear MOD 400 = 0)
end;

procedure MonthsInit;
var
  I: Integer;
BEGIN
  DaysInMonth[01] := 31;
  DaysInMonth[02] := 28;  (* adjust for leap years later *)
  DaysInMonth[03] := 31;
  DaysInMonth[04] := 30;
  DaysInMonth[05] := 31;
  DaysInMonth[06] := 30;
  DaysInMonth[07] := 31;
  DaysInMonth[08] := 31;
  DaysInMonth[09] := 30;
  DaysInMonth[10] := 31;
  DaysInMonth[11] := 30;
  DaysInMonth[12] := 31;

  DaysBeforeMonth[01] := 0;
  for I := 01 to 11 do
  begin
    DaysBeforeMonth[I + 1] := DaysBeforeMonth[I] + DaysInMonth[I];
  end;
end;

begin
  MonthsInit;
end.

