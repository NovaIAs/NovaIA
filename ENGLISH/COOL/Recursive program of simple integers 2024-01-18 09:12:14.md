MODULE Main;

IMPORT
Out;

VAR
magic: INTEGER := 666;

PROCEDURE hello(x: INTEGER);
BEGIN
    Out.Int(x + magic, 0);
    Out.Ln
END hello;

PROCEDURE callsSelf(x: INTEGER);
BEGIN
    IF x > 0 THEN
        callsSelf(x-1)
    FI;
    Out.Int(x + magic, 0);
    Out.Ln
END callsSelf;

BEGIN
    hello(20);
    Out.String("magic = ");
    Out.Int(magic, 0);
    Out.Ln;
    Out.String("callsSelf(15) = ");
    callsSelf(15);
    Out.Ln;
    Out.String("callsSelf(-1) = ");
    callsSelf(-1)
END Main.