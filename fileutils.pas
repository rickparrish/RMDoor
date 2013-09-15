unit FileUtils;

{$mode objfpc}{$H+}

interface

function LockFile(handle, start, length: LongInt): LongInt;
function UnLockFile(handle, start, length: LongInt): LongInt;

implementation

function LockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils LockFile'); Halt;
end;

function UnLockFile(handle, start, length: LongInt): LongInt;
begin
  WriteLn('REEPORT FileUtils UnLockFile'); Halt;
end;

end.

