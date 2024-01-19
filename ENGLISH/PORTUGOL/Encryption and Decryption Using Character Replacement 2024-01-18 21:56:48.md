```portangol
PROGRAM PROJETO_COMPLEXO;
PROCEDURE NUMERO_APERECIMENTO (NUMERO, SEQ);
    VAR CONT, POS, APARECIMENTO;
BEGIN
    POS := 1;
    CONT := 0;
    APARECIMENTO := 0;

    WHILE (POS <= LENGTH(SEQ)) DO BEGIN
        IF NUMERO = SUBSTRING(SEQ, POS, 1) THEN BEGIN
            CONT := CONT + 1;
            APARECIMENTO := POS;
        END;
        POS := POS + 1;
    END;

    RETURN (CONT, APARECIMENTO);
END;

PROCEDURE ENCRIPTAR_TEXTO (TEXTO, CHAVE, TEXTO_ENCRIPTADO);
    VAR I, TAM_CHAVE, NUMERO;
BEGIN
        TAM_CHAVE := LENGTH(CHAVE);
        TEXTO_ENCRIPTADO := "";
        
        FOR I := 1 TO LENGTH(TEXTO) DO BEGIN
        NUMERO := NUMERO_APERECIMENTO(SUBSTRING(TEXTO, I, 1), CHAVE);

        IF NUMERO.CONT > 0 THEN BEGIN
            TEXTO_ENCRIPTADO := TEXTO_ENCRIPTADO + SUBSTRING(CHAVE, NUMERO.APARECIMENTO, 1);
        ELSE BEGIN
            TEXTO_ENCRIPTADO := TEXTO_ENCRIPTADO + SUBSTRING(TEXTO, I, 1);
        END;
    END;
END;

PROCEDURE DECRIPTAR_TEXTO (TEXTO_ENCRIPTADO, CHAVE, TEXTO_DECRIPTADO);
    VAR I, TAM_CHAVE, NUMERO;
BEGIN
    TAM_CHAVE := LENGTH(CHAVE);
    TEXTO_DECRIPTADO := "";

    FOR I := 1 TO LENGTH(TEXTO_ENCRIPTADO) DO BEGIN
        NUMERO := NUMERO_APERECIMENTO(SUBSTRING(TEXTO_ENCRIPTADO, I, 1), CHAVE);

        IF NUMERO.CONT > 0 THEN BEGIN
            TEXTO_DECRIPTADO := TEXTO_DECRIPTADO + SUBSTRING(CHAVE, NUMERO.APARECIMENTO, 1);
        ELSE BEGIN
            TEXTO_DECRIPTADO := TEXTO_DECRIPTADO + SUBSTRING(TEXTO_ENCRIPTADO, I, 1);
        END;
    END;
END;

VAR TEXTO, CHAVE, TEXTO_ENCRIPTADO, TEXTO_DECRIPTADO;

TEXTO := "OLA, MUNDO!";
CHAVE := "SECRET";

ENCRIPTAR_TEXTO(TEXTO, CHAVE, TEXTO_ENCRIPTADO);
DECRIPTAR_TEXTO(TEXTO_ENCRIPTADO, CHAVE, TEXTO_DECRIPTADO);

PRINT(TEXTO_ENCRIPTADO);
PRINT(TEXTO_DECRIPTADO);

END.
```

This code is a complex program written in PORTUGOL that implements a simple encryption and decryption algorithm using a key. It defines two procedures, `NUMERO_APERECIMENTO` and `ENCRIPTAR_TEXTO` and `DECRIPTAR_TEXTO`, and uses them in the main program.

* `NUMERO_APERECIMENTO` takes two parameters, a number and a sequence, and returns the number of times the number appears in the sequence and the position of its first occurrence.
* `ENCRIPTAR_TEXTO` takes three parameters, the text to encrypt, the encryption key, and a variable to store the encrypted text. It iterates over the input text character by character, and for each character, it checks if the character is in the key. If it is, it replaces the character with the character in the key at the same position. Otherwise, it leaves the character as it is.
* `DECRIPTAR_TEXTO` takes three parameters, the encrypted text, the encryption key, and a variable to store the decrypted text. It iterates over the input text character by character, and for each character, it checks if the character is in the key. If it is, it replaces the character with the character in the key at the same position. Otherwise, it leaves the character as it is.

The main program defines two variables, `TEXTO` and `CHAVE`, and then calls the `ENCRIPTAR_TEXTO` procedure to encrypt the text with the key, and the `DECRIPTAR_TEXTO` procedure to decrypt the encrypted text with the key.

The output of the program is the encrypted text and the decrypted text, which are printed to the console.