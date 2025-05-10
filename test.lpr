program Test;

{$mode objfpc}{$h+}

uses
  Crt, Door, SysUtils;

var
  Ch: Char;
  I: Integer;
  S: String;

begin
  try
    try
      DoorStartUp();

      DoorClrScr();

      DoorWriteLn('DropInfo.Alias = ' + DoorDropInfo.Alias);
      DoorWriteLn('DropInfo.RealName = ' + DoorDropInfo.RealName);
      DoorWriteLn('DropInfo.AliasOrRealName = ' + DoorDropInfo.AliasOrRealName);
      DoorWriteLn('DropInfo.RealNameOrAlias = ' + DoorDropInfo.RealNameOrAlias);
      DoorWriteLn;

      DoorWriteLn('This is written with DoorWriteLn()');
      WriteLn('This is written with WriteLn()');

      DoorWriteLn();
      DoorWrite('Input Test: ');
      S := DoorInput('Type something here', DOOR_INPUT_CHARS_ALPHA + ' ', #0, 20, 40, 31);
      DoorWriteLn('You typed: ' + S);
      DoorWriteLn();

      DoorWriteLn('WindMaxX=' + IntToStr(WindMaxX) + ', WindMaxY=' + IntToStr(WindMaxY));
      DoorWriteLn();

      DoorWriteLn('Testing 8 background colours:');
      for I := 0 to 7 do
      begin
        DoorTextBackground(I);
        if (I = 0) then
        begin
          DoorTextColour(15);
        end else
        begin
          DoorTextColour(0);
        end;
        DoorWriteLn('Test Background #' + IntToStr(I));
      end;
      DoorTextAttr(7);
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

