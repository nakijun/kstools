unit ksRegTools;

interface

uses
  Classes, ksComm;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ksTools', [TksComPort]);
end;

end.
