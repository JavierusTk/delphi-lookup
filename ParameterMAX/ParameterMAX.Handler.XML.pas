unit ParameterMAX.Handler.XML;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, Xml.Internal.OmniXML,
  ParameterMAX.Handlers, ParameterMAX.HandlerRegistry;

type
  TXMLParameterHandler = class(TParameterHandlerBase)
  private
    procedure ProcessXMLNode(Node: OmniIXMLNode; const Prefix: string = '');
    function XMLValueToString(const Value: string): string;
  public
    function LoadParametersInternal(const FileName: string): Boolean; override;
  end;

implementation

{ TXMLParameterHandler }

function TXMLParameterHandler.LoadParametersInternal(const FileName: string): Boolean;
var
  XMLDoc: OmniIXMLDocument;
  RootNode: OmniIXMLElement;
begin
  try
    XMLDoc := CreateXMLDoc;
    
    if XMLDoc.Load(FileName) then
    begin
      RootNode := XMLDoc.DocumentElement;
      if Assigned(RootNode) then
      begin
        FParameters.Clear;
        ProcessXMLNode(RootNode);
        Result := True;
      end
      else
        Result := False;
    end
    else
      Result := False;
  except
    Result := False;
  end;
end;

procedure TXMLParameterHandler.ProcessXMLNode(Node: OmniIXMLNode; const Prefix: string);
var
  i: Integer;
  Key, Value, FullKey: string;
  ChildNode: OmniIXMLNode;
begin
  if not Assigned(Node) then
    Exit;
  
  // Process child nodes
  if Node.ChildNodes.Length > 0 then
  begin
    for i := 0 to Node.ChildNodes.Length - 1 do
    begin
      ChildNode := Node.ChildNodes.Item[i];
      
      if ChildNode.NodeType = ELEMENT_NODE then
      begin
        Key := ChildNode.NodeName;
        
        // Build full key with prefix
        if Prefix <> '' then
          FullKey := Prefix + '.' + Key
        else
          FullKey := Key;
        
        // Check if this element has text content (no child elements or only text nodes)
        if (ChildNode.ChildNodes.Length = 0) or 
           ((ChildNode.ChildNodes.Length = 1) and (ChildNode.ChildNodes.Item[0].NodeType = TEXT_NODE)) then
        begin
          // Extract element text content as parameter value
          Value := Trim(ChildNode.Text);
          if Value <> '' then
          begin
            Value := XMLValueToString(Value);
            FParameters.AddOrSetValue(LowerCase(FullKey), Value);
          end;
        end
        else
        begin
          // Recursively process child elements with the current full key as prefix
          ProcessXMLNode(ChildNode, FullKey);
        end;
      end;
    end;
  end;
end;

function TXMLParameterHandler.XMLValueToString(const Value: string): string;
var
  LowerValue: string;
begin
  LowerValue := LowerCase(Trim(Value));
  
  if (LowerValue = 'true') or (LowerValue = 'yes') or (LowerValue = '1') then
    Result := '1'
  else if (LowerValue = 'false') or (LowerValue = 'no') or (LowerValue = '0') then
    Result := '0'
  else if LowerValue = 'null' then
    Result := ''
  else
    Result := Trim(Value);
end;

initialization
  TParameterHandlerRegistry.Instance.RegisterHandler(TXMLParameterHandler, '.xml');

end.