unit Connection.Intf;

interface

type
  IConnectionContent = interface
    procedure SetInformation(const Text: string);
    procedure SetException(const Text: string);
    procedure SetOK(const Text: string);
  end;

implementation

end.
