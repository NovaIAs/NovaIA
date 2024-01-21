```forth

: CONFIGURAR-PIC16 ( DIRECCION-PIC16 -- )
    0         [CHAR] PIC16-CONECTADO @ SWAP '='
    FIN-NUMERO {" ¡Error en la configuración del PIC16!" } IF
    0         [CHAR] PIC16-COMANDO @ SWAP '='
    FIN-NUMERO {" ¡Error en la configuración del PIC16!" } IF
    0         [CHAR] PIC16-IN @ SWAP '='
    FIN-NUMERO {" ¡Error en la configuración del PIC16!" } IF
    0         [CHAR] PIC16-OUT @ SWAP '='
    FIN-NUMERO {" ¡Error en la configuración del PIC16!" } IF
    ' $(PIC16-CONECTADO) PIC16-NAME !
    ' $(PIC16-COMANDO) PIC16-COMMAND !
    ' $(PIC16-IN) PIC16-IN !
    ' $(PIC16-OUT) PIC16-OUT ! ;

: VARIABLE ( NOMBRE CANTIDAD-BITS DIRECCION-INICIO -- )
    { ' $(NOMBRE)
    { ' $(CANTIDAD-BITS) B
    ' $(DIRECCION-INICIO) @ SWAP
    { ' $(NOMBRE)
    { ' $(CANTIDAD-BITS)
    ' $(DIRECCION-INICIO) + } VARIABLES !
    ' $(NOMBRE)
    ' $(CANTIDAD-BITS)
    ' $(DIRECCION-INICIO) + } VARIABLES ! ;

: CHECKSUM ( INICIO FINAL -- )
    ACCUM 0 !
    { ' $(INICIO) ' $(FINAL) DO I
    { I @ @ ACCUM + } LOOP
    ACCUM @ } LOOP
    ACCUM @ ;

: WRITE-ARRAY ( DIRECCION CANTIDAD -- )
    VARIABLE.PIC16 PIC16-OUT @
    { ' $(CANTIDAD) 1- BYTE } REPEAT
    { ' $(DIRECCION) TYPE } LOOP ;

: WRITE-STRING ( DIRECCION STRING -- )
    STRING COUNT
    VARIABLE.PIC16 PIC16-OUT @
    { ' $(STRING) TYPE } LOOP ;

: PRINT ( PALABRA -- )
    PALABRA @ VARIABLE.PIC16 PIC16-IN @
    { ' $(PALABRA) ." " } TYPE } LOOP ;
    CR ;

: CONFIRMAR ( MSJ -- )
    MSJ TYPE
    CR "  (N/S): " TYPE
    KEY ACCEPT
    DROP
    'N' NEGATE ;

: CODIFICAR-ID ( NUMERO -- )
    65 + DUP @ SWAP 45 - SWAP 57 -
    CODIGO 3 OVER + @ SWAP DROP SWAP @
    { I @ @ DUP @ OVER SWAP CHAR+ DROP } LOOP ;

```