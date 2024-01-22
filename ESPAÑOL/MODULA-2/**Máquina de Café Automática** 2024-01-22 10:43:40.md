```modula-2

MODULE MaquinaDeCafe(

    IMPORT
        TextIO;
        Modulos.Lcd;
        Modulos.Sensores;

    CONST
        numBebidas = 5;  -- Número de bebidas disponibles

    TYPE
        tBebida = RECORD
            precio: CARDINAL;
            nombre: ARRAY 16 OF CHAR;
            ingredientes: ARRAY numIngredientes OF CHAR;
        END;

        tMaquinaDeCafe = RECORD
            bebidas: ARRAY numBebidas OF tBebida;
            dinero: CARDINAL;
            lcd: Modulos.Lcd.tDisplay;
            sensores: Modulos.Sensores.tSensores;
        END;

    VAR
        maquina: tMaquinaDeCafe;

    PROCEDURE InicializarMaquina;
    VAR
        i: CARDINAL;
    BEGIN
        maquina.bebidas[1].nombre := "Café solo";
        maquina.bebidas[1].precio := 100;
        maquina.bebidas[1].ingredientes := ["Café", "Agua"];

        maquina.bebidas[2].nombre := "Café con leche";
        maquina.bebidas[2].precio := 150;
        maquina.bebidas[2].ingredientes := ["Café", "Leche", "Agua"];

        maquina.bebidas[3].nombre := "Chocolate";
        maquina.bebidas[3].precio := 120;
        maquina.bebidas[3].ingredientes := ["Chocolate", "Leche", "Agua"];

        maquina.bebidas[4].nombre := "Té";
        maquina.bebidas[4].precio := 80;
        maquina.bebidas[4].ingredientes := ["Té", "Agua"];

        maquina.bebidas[5].nombre := "Refresco";
        maquina.bebidas[5].precio := 100;
        maquina.bebidas[5].ingredientes := ["Refresco"];

        maquina.dinero := 0;

        maquina.lcd.Clear;
        TextIO.PutString(maquina.lcd, "Bienvenido a la máquina de café");
    END InicializarMaquina;

    PROCEDURE MostrarBebidas;
    VAR
        i: CARDINAL;
    BEGIN
        maquina.lcd.Clear;
        TextIO.PutString(maquina.lcd, "Bebidas disponibles:");
        FOR i:=1 TO numBebidas DO
            TextIO.PutString(maquina.lcd, maquina.bebidas[i].nombre);
            TextIO.PutInt(maquina.lcd, maquina.bebidas[i].precio, 0);
            TextIO.PutChar(maquina.lcd, 10);  -- Salto de línea
        END;
    END MostrarBebidas;

    PROCEDURE SeleccionarBebida;
    VAR
        bebida: CARDINAL;
    BEGIN
        maquina.lcd.Clear;
        TextIO.PutString(maquina.lcd, "Elija una bebida:");

        FOR i:=1 TO numBebidas DO
            TextIO.PutInt(maquina.lcd, i, 0);
            TextIO.PutString(maquina.lcd, ". ");
            TextIO.PutString(maquina.lcd, maquina.bebidas[i].nombre);
            TextIO.PutChar(maquina.lcd, 10);  -- Salto de línea
        END;

        TextIO.ReadCard(maquina.lcd, bebida);

        IF (bebida >= 1) AND (bebida <= numBebidas) THEN
            PrepararBebida(bebida);
        ELSE
            maquina.lcd.Clear;
            TextIO.PutString(maquina.lcd, "Bebida no válida");
        END;
    END SeleccionarBebida;

    PROCEDURE PrepararBebida(bebida: CARDINAL);
    VAR
        i: CARDINAL;
    BEGIN
        maquina.lcd.Clear;
        TextIO.PutString(maquina.lcd, "Preparando ");
        TextIO.PutString(maquina.lcd, maquina.bebidas[bebida].nombre);

        FOR i:=1 TO numIngredientes DO
            Modulos.Sensores.EncenderSensor(maquina.sensores, i);
            DELAY 500;
            Modulos.Sensores.ApagarSensor(maquina.sensores, i);
        END;

        TextIO.PutString(maquina.lcd, "Bebida lista");

        maquina.dinero := maquina.dinero + maquina.bebidas[bebida].precio;
    END PrepararBebida;

    PROCEDURE InsertarDinero;
    VAR
        cantidad: CARDINAL;
    BEGIN
        maquina.lcd.Clear;
        TextIO.PutString(maquina.lcd, "Inserte dinero:");

        TextIO.ReadCard(maquina.lcd, cantidad);

        IF cantidad > 0 THEN
            maquina.dinero := maquina.dinero + cantidad;

            TextIO.PutString(maquina.lcd, "Dinero insertado");
        ELSE
            TextIO.PutString(maquina.lcd, "Cantidad inválida");
        END;
    END InsertarDinero;

    PROCEDURE DevolverDinero;
    BEGIN
        maquina.lcd.Clear;
        TextIO.PutString(maquina.lcd, "Devolviendo dinero:");
        TextIO.PutInt(maquina.lcd, maquina.dinero, 0);

        maquina.dinero := 0;
    END DevolverDinero;

BEGIN
    InicializarMaquina;

    WHILE TRUE DO
        MostrarBebidas;

        SELECT
            WHEN maquina.sensores.boton1 THEN
                SeleccionarBebida;

            WHEN maquina.sensores.boton2 THEN
                InsertarDinero;

            WHEN maquina.sensores.boton3 THEN
                DevolverDinero
        END;
    END;
END MaquinaDeCafe.

```