```modula-2
MODULE Network;
FROM Terminal IMPORT Write;

TYPE
    Buffer = ARRAY 0 .. 1023 OF CHAR;
    Message = RECORD
        Seq: INTEGER;
        Buf: BUFFER;
        Len: INTEGER;
    END;

VAR
    Buffer: BUFFER;
    Msg: MESSAGE;
    NextSeq = 0;

PROCEDURE SendMessage(Message: MESSAGE);
VAR
    B: BYTE;
BEGIN
    B := Message.Seq + (Message.Len >> 8);
    Write(B);
    B := Message.Len MOD 256;
    Write(B);
    FOR I := 0 TO Message.Len - 1 DO
        Write(Message.Buf[I])
    END
END SendMessage;

PROCEDURE ReceiveMessage(VAR Message: MESSAGE);
VAR
    B: BYTE;
BEGIN
    Read(B);
    Message.Seq := B - (Message.Len >> 8);
    Read(B);
    Message.Len := B + (Message.Seq << 8);
    FOR I := 0 TO Message.Len - 1 DO
        Read(Message.Buf[I])
    END
END ReceiveMessage;

PROCEDURE SendData(Data: STRING);
VAR
    Length: INTEGER;
BEGIN
    Length := ORD(Data[0]) + (ORD(Data[1]) << 8);
    Msg.Len := Length;
    Msg.Seq := NextSeq;
    NextSeq := NextSeq + 1;
    FOR I := 0 TO Length - 1 DO
        Msg.Buf[I] := Data[I + 2]
    END;
    SendMessage(Msg)
END SendData;

PROCEDURE ReceiveData(VAR Data: STRING);
VAR
    Length: INTEGER;
BEGIN
    ReceiveMessage(Msg);
    Length := Msg.Len;
    FOR I := 0 TO Length - 1 DO
        Data[I + 2] := Msg.Buf[I]
    END;
    Data[0] := CHR(Length);
    Data[1] := CHR(Length SHR 8)
END ReceiveData;

BEGIN
    ReadTerminal("Hello, world!");
    ReceiveData(Buffer);
    ReadTerminal(Buffer[2 .. ORD(Buffer[0]) + ORD(Buffer[1]) << 8]):
END Network.
```

This code implements a simple network protocol in MODULA-2. It defines a type for messages, which includes a sequence number, a buffer, and a length. It also defines procedures for sending and receiving messages, as well as a procedure for sending data.

The `SendMessage` procedure takes a message as an argument and sends it over the network. It first sends the message's sequence number and length, followed by the message's buffer.

The `ReceiveMessage` procedure receives a message from the network. It first reads the message's sequence number and length, then reads the message's buffer.

The `SendData` procedure takes a string of data as an argument and sends it over the network. It first converts the string's length to a 16-bit integer, then creates a message with the appropriate sequence number, length, and buffer. Finally, it sends the message over the network using the `SendMessage` procedure.

The `ReceiveData` procedure receives a string of data from the network. It first receives a message from the network using the `ReceiveMessage` procedure. It then converts the message's length to a 16-bit integer, and copies the message's buffer into the string.

The main program begins by sending the message "Hello, world!" to the network. It then receives a message from the network and prints it to the terminal.