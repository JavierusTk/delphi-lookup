unit UnicodeProbe;

interface

type
  /// Clase de prueba: umbral τ ≥ 0,8 → estado ✅ (caña, 5 €)
  TUnicodeProbe = class
  public
    // MT-BRIDGE: comentario con flecha → eliminar después
    function Umbral: string;
  end;

const
  ProbeTexto = 'τ → ≥ ✅ ñ €';

implementation

{ TUnicodeProbe }

function TUnicodeProbe.Umbral: string;
begin
  // dentro del cuerpo: τ → ≥
  Result := ProbeTexto;
end;

end.
