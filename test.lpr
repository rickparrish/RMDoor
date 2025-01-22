program Test;

{$mode objfpc}{$h+}

uses
  Door, SysUtils;

var
  Ch: Char;
  S: String;

begin
  try
    try
      DoorStartUp();

      DoorClrScr();
      DoorWriteLn('This is written with DoorWriteLn()');
      WriteLn('This is written with WriteLn()');

      DoorWriteLn();
      DoorWrite('Input Test: ');
      S := DoorInput('Type something here', DOOR_INPUT_CHARS_ALPHA + ' ', #0, 20, 40, 31);
      DoorWriteLn('You typed: ' + S);

      DoorWriteLn();
      DoorWriteLn('Hit Q to Quit');
      repeat
        Ch := DoorReadKey();
        DoorWriteLn('You pressed: ' + Ch);
      until (UpCase(Ch) = 'Q');
    except
      on E: Exception do
      begin
        DoorSession.SethWrite := True;
        DoorWriteLn();
        DoorWriteLn('`4`b**`% ERROR : `2' + E.Message + ' `4`b**`2');
        DoorWrite('Hit a key to quit');
        DoorReadKey();
      end;
    end;
  finally
    DoorShutDown();
  end;
end.

