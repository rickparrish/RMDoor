unit FileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function CopyFile(ASourceFileName, ADestFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
procedure DumpExceptionCallStack(var F: Text; E: Exception);
function LockFile(handle, start, length: LongInt): LongInt;
function OpenFileForAppend(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForOverwrite(out F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForOverwrite(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForRead(out F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForReadWrite(out F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function ReadFile(AFileName: String; var ASL: TStringList; ATimeoutInMilliseconds: Integer): Boolean;
function RenameFile(AOldFileName, ANewFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function TrimTopLines(AFileName: String; ALineCount, ATimeoutInMilliseconds: Integer): Boolean;
function UnLockFile(handle, start, length: LongInt): LongInt;
function WriteFile(AFileName: String; var ASL: TStringList; ATimeoutInMilliseconds: Integer): Boolean;

implementation

function CopyFile(ASourceFileName, ADestFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  Buffer: Array[1..1024] of Byte;
  DestF, SourceF: File;
  NumRead, NumWritten: LongInt;
begin
  Result := false;

  if ((ASourceFileName <> '') AND (ADestFileName <> '') AND (FileExists(ASourceFileName))) then
  begin
    if (OpenFileForReadWrite(SourceF, ASourceFileName, 2500)) then
    begin
      if (OpenFileForOverwrite(DestF, ADestFileName, 2500)) then
      begin
        Buffer[1] := 0; // Make the "does not seem to be initialized" hint go away
        NumRead := 0;
        NumWritten := 0;

        repeat
          BlockRead(SourceF, Buffer, SizeOf(Buffer), NumRead);
          BlockWrite(DestF, Buffer, NumRead, NumWritten);

          if (NumRead <> NumWritten) then
          begin
            // This shouldn't happen, but we should bail instead of retrying if it does
            Exit;
          end;
        until (NumRead = 0);

        Close(SourceF);
        Close(DestF);

        Result := true;
        Exit;
      end else
      begin
        Close(SourceF);
      end;
    end;
  end;
end;

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

  // TODOX Race condition
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
    {$I-}ReWrite(F, 1);{$I+}
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

  // TODOX Race condition
  if (FileExists(AFileName)) then
  begin
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
  end else
  begin
    Result := OpenFileForOverwrite(F, AFileName, ATimeoutInMilliseconds);
  end;
end;

function ReadFile(AFileName: String; var ASL: TStringList; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  for I := 1 to ATimeoutInMilliseconds div 100 do
  begin
    try
      ASL.LoadFromFile(AFileName);
      Result := true;
      Exit;
    except
      on E: Exception do
      begin
        Sleep(100); // Wait 1/10th of a second before retrying
      end;
    end;
  end;
end;

function RenameFile(AOldFileName, ANewFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  if ((AOldFileName <> '') AND (ANewFileName <> '') AND (FileExists(AOldFileName))) then
  begin
    for I := 1 to ATimeoutInMilliseconds div 100 do
    begin
      if (SysUtils.RenameFile(AOldFileName, ANewFileName)) then
      begin
        Result := true;
        Exit;
      end;

      Sleep(100); // Wait 1/10th of a second before retrying
    end;
  end;
end;

function TrimTopLines(AFileName: String; ALineCount, ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
  SL: TStringList;
begin
  Result := false;

  if (FileExists(AFileName)) then
  begin
    for I := 1 to ATimeoutInMilliseconds div 100 do
    begin
      try
        SL := TStringList.Create;
        SL.LoadFromFile(AFileName);
        if (SL.Count > ALineCount) then
        begin
          while (SL.Count > ALineCount) do
          begin
            SL.Delete(0);
          end;
          SL.SaveToFile(AFileName);
          SL.Free;

          Result := true;
          Exit;
        end;
      except
        on E: Exception do
        begin
          Sleep(100); // Wait 1/10th of a second before retrying
        end;
      end;
    end;
  end;
end;

function UnLockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils UnLockFile'); Halt;
  Result := 0;
end;

function WriteFile(AFileName: String; var ASL: TStringList; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;

  for I := 1 to ATimeoutInMilliseconds div 100 do
  begin
    try
      ASL.SaveToFile(AFileName);
      Result := true;
      Exit;
    except
      on E: Exception do
      begin
        Sleep(100); // Wait 1/10th of a second before retrying
      end;
    end;
  end;
end;

end.

