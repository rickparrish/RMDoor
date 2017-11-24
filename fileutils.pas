unit FileUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

procedure DumpExceptionCallStack(var F: Text; E: Exception);
function LockFile(handle, start, length: LongInt): LongInt;
function OpenFileForAppend(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForOverwrite(out F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForOverwrite(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForRead(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForReadWrite(out F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function UnLockFile(handle, start, length: LongInt): LongInt;

implementation

// From: http://wiki.freepascal.org/Logging_exceptions#Dump_exception_call_stack
procedure DumpExceptionCallStack(var F: Text; E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  WriteLn(F, Report);
end;

function LockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils LockFile'); Halt;
  Result := 0;
end;

function OpenFileForAppend(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  if (FileExists(AFileName)) then
  begin
    for I := 1 to ATimeoutInMilliseconds div 100 do
    begin
      Assign(F, AFileName);
      {$I-}Append(F);{$I+}
      if (IOResult = 0) then
      begin
        Result := true;
        Exit;
      end;

      Sleep(100); // Wait 1/10th of a second before retrying
    end;
  end else
  begin
    Result := OpenFileForOverwrite(F, AFileName, ATimeoutInMilliseconds);
  end;
end;

function OpenFileForOverwrite(out F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  for I := 1 to ATimeoutInMilliseconds div 100 do
  begin
    Assign(F, AFileName);
    {$I-}ReWrite(F);{$I+}
    if (IOResult = 0) then
    begin
      Result := true;
      Exit;
    end;

    Sleep(100); // Wait 1/10th of a second before retrying
  end;
end;

function OpenFileForOverwrite(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  for I := 1 to ATimeoutInMilliseconds div 100 do
  begin
    Assign(F, AFileName);
    {$I-}ReWrite(F);{$I+}
    if (IOResult = 0) then
    begin
      Result := true;
      Exit;
    end;

    Sleep(100); // Wait 1/10th of a second before retrying
  end;
end;

function OpenFileForRead(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  for I := 1 to ATimeoutInMilliseconds div 100 do
  begin
    Assign(F, AFileName);
    {$I-}Reset(F);{$I+}
    if (IOResult = 0) then
    begin
      Result := true;
      Exit;
    end;

    Sleep(100); // Wait 1/10th of a second before retrying
  end;
end;

function OpenFileForReadWrite(out F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  for I := 1 to ATimeoutInMilliseconds div 100 do
  begin
    Assign(F, AFileName);
    {$I-}Reset(F, 1);{$I+}
    if (IOResult = 0) then
    begin
      Result := true;
      Exit;
    end;

    Sleep(100); // Wait 1/10th of a second before retrying
  end;
end;

function UnLockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils UnLockFile'); Halt;
  Result := 0;
end;

end.

