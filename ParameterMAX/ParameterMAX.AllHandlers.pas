unit ParameterMAX.AllHandlers;

{
  This unit provides convenient access to ALL parameter handlers including:
  
  File format handlers:
  - JSON (.json)
  - XML (.xml) 
  - INI (.ini)
  - YAML (.yaml, .yml)
  - TXT (.txt) - Simple key=value format
  
  Fallback handlers:
  - Environment variables (always available)
  - Windows Registry (Windows only, via conditional compilation)
  
  Simply include this unit in your uses clause to automatically register
  all handlers. If you only need specific handlers, you can include them
  individually instead of using this unit to reduce your application's size.
}

interface

implementation

uses
  // File format handlers
  ParameterMAX.Handler.JSON,
  ParameterMAX.Handler.XML,
  ParameterMAX.Handler.INI,
  ParameterMAX.Handler.YAML,
  ParameterMAX.Handler.TXT,
  
  // Fallback handlers
  ParameterMAX.Environment
  {$IFDEF MSWINDOWS}
  , ParameterMAX.Registry  // Windows Registry support (Windows only)
  {$ENDIF}
  ;

end.