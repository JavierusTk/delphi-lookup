unit OneLineCond;

// T-5AF2: one-line conditionals must not leave the conditional depth open,
// or the unit is indexed whole instead of interface-only.

interface

{$IFDEF NEVER_DEFINED} uses SysUtils; {$ENDIF}

procedure OneLineVisible(Val: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif});

implementation

procedure OneLineHiddenHelper;
begin
end;

procedure OneLineVisible(Val: Integer);
begin
  OneLineHiddenHelper;
end;

end.
