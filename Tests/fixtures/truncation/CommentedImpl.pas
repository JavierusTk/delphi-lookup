unit CommentedImpl;

// T-5AF2: an 'implementation' line inside a comment does not end the
// interface, and directive-like text in strings or comments is not counted.

interface

{ Notes:
implementation
  the line above is inside a comment }

const
  CommentedFakeDirective = '{$IFDEF X}';   // {$IFDEF Y}

procedure CommentedVisible;

implementation

procedure CommentedHiddenHelper;
begin
end;

procedure CommentedVisible;
begin
  CommentedHiddenHelper;
end;

end.
