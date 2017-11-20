unit FileUtils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

function LockFile(handle, start, length: LongInt): LongInt;
function OpenFileForAppend(var F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForOverwrite(var F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForOverwrite(var F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForRead(var F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function OpenFileForReadWrite(var F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
function UnLockFile(handle, start, length: LongInt): LongInt;

implementation

function LockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils LockFile'); Halt;
end;

function OpenFileForAppend(var F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
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

  Result := false;
end;

function OpenFileForOverwrite(var F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
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

  Result := false;
end;

function OpenFileForOverwrite(var F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
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

  Result := false;
end;

function OpenFileForRead(var F: Text; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
begin
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

  Result := false;
end;

function OpenFileForReadWrite(var F: File; AFileName: String; ATimeoutInMilliseconds: Integer): Boolean;
var
  I: Integer;
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

  Result := false;
end;

function UnLockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils UnLockFile'); Halt;
end;

end.

