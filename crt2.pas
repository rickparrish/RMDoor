unit Crt2;

{$mode objfpc}{$h+}

interface

function GetTextAttr: Byte;
procedure SetTextAttr(AAttr: Byte);
procedure SetTextColour(AColour: Byte);
procedure SetTextBackground(AColour: Byte);

implementation

uses
  Crt;

var
  FTextAttr: Byte;

function GetTextAttr: Byte;
begin
  Result := FTextAttr;
end;

procedure SetTextAttr(AAttr: Byte);
begin
  Crt.TextAttr := AAttr;
  FTextAttr := AAttr;
end;

procedure SetTextColour(AColour: Byte);
begin
  Crt.TextColor(AColour);

  //  From https://github.com/newpascal/freepascal/blob/master/packages/rtl-console/src/win/crt.pp
  FTextAttr := (AColour AND $8F) OR (FTextAttr AND $70);
end;

procedure SetTextBackground(AColour: Byte);
begin
  Crt.TextBackground(AColour);

  //  From https://github.com/newpascal/freepascal/blob/master/packages/rtl-console/src/win/crt.pp
  FTextAttr := ((AColour SHL 4) AND ($F0 AND NOT Crt.Blink)) OR (FTextAttr AND ($0F OR Crt.Blink));
end;

begin
  if (Crt.TextAttr > 0) then
  begin
    FTextAttr := TextAttr;
  end else
  begin
    FTextAttr := Crt.LightGray;
  end;
end.
