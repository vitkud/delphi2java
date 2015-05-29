unit TestUnitU;

interface

uses
  Classes;

const
  TEST_CONST_INT = 1024 * 100;
  TEST_CONST_STRING = 'string';

type

  ETestError = class (Exception);

  ITestInterface1 = interface (IUnknown)
  ['{1FFD5A2B-B773-4D48-AAD5-C09923C21FA7}']
    function TestFunction1(const Param1, Param2: string): string;
    function TestFunction2: string;
  end;

  ITestInterface2 = interface (ITestInterface1)
  ['{C268BB58-6326-4985-9774-F1BD183350A8}']
  end;

implementation

initialization

end.