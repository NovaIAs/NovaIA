```portugal
*PROGRAMA PRINCIPAL
*VARIAVEIS
   IA, JA: INTEIRO;
   SEA,SEAUM,SEAM,SEAAUM: REAL;
   V: VETOR[0:6] DE BYTE;
   ENTRA, VERDE, VERMELHO, AZUL: COR;

*CONSTANTES
   PRETO←0;
   BRANCO←255;
   CINZA_ESCURO←128;
   CINZA_CLARO←192;
   VERMELHO←AZUL;

*PROCEDIMENTOS

   INCIALIZA_CORES();
   INCIALIZA_VETOR();
   Efetua_Video_40_Linhas();

*FUNCOES
   COR FUN_EXTRACT_COR(COR COR1, COR COR2) LOCAL;
   COR RESULTADO;
   COR_MEDIA(COR1, COR2, RESULTADO);
   FUN_EXTRACT_COR←RESULTADO;
   FIM_FUN;

*FUNCOES
   COR FUN_MEDIA_COR(COR COR1, COR COR2) LOCAL;
   COR RESULTADO;
   COR_MEDIA(COR1, COR2, RESULTADO);
   FUN_MEDIA_COR←RESULTADO;
   FIM_FUN;

*FUNCOES
   COR FUN_SOMBRIA_COR(COR COR1) LOCAL;
   COR RESULTADO;
   RESULTADO←COR_SOMBRIA(COR1);
   FUN_SOMBRIA_COR←RESULTADO;
   FIM_FUN;

*FUNCOES
   COR FUN_BRILHANTE_COR(COR COR1) LOCAL;
   COR RESULTADO;
   RESULTADO←COR_BRILHANTE(COR1);
   FUN_BRILHANTE_COR←RESULTADO;
   FIM_FUN;

*PROCEDIMENTOS
   PROC INCIALIZA_CORES() LOCAL;
   BYTE CURSOR;
   CURSOR←CURSOR_DESLIGADO;
   SET_COR_CURSOR(CURSOR,BRANCO);
   CURSOR_LENTO(FALSO);
   CLEAR_COLOR_SCREEN(CINZA_ESCURO);
   CLEAR_COLOR_PALET(PRETO);
   FIM_PROC;

*PROCEDIMENTOS
   PROC INCIALIZA_VETOR() LOCAL;
   IA←0;
   SEAM←0;
   PARA JA←0 ATE 6 FAZER
   SEA←JA*(255/6);
   V[JA]←BYTE(SEA);
   FIM_PARA;
   FIM_PROC;

*PROCEDIMENTOS
   PROC Efetua_Video_40_Linhas() LOCAL;
   BYTE COR1, COR2, PALET[0:63];
   INTEIRO I, WI, WS, WT, WH;
   SET_VIDEO_MODE(640, 480, 16);
   FOR I←0 TO 63 DO
   PALET[I]←0;
   FIM_FOR;
   PALET[0]←PRETO;
   PALET[1]←CINZA_ESCURO;
   PALET[2]←CINZA_CLARO;
   PALET[3]←BRANCO;
   PALET[4]←VERMELHO;
   PALET[5]←VERDE;
   PALET[6]←AZUL;
   PARA JA←0 ATE 6 FAZER
   COR1←BYTE(SEA);
   COR2←BYTE(SEAM);
   PARA I←0 ATE 63 FAZER
   PALET[I+8]←BYTE(SEAUM);
   PALET[I+16]←BYTE(SEAAUM);
   FIM_PARA;
   PARA I←0 ATE 40 FAZER
   WI←I MOD 8;
   WS←I DIV 8;
   WT←WI*80;
   WH←WS*480;
   SELECT WI DO
   CASO 0, 2, 4, 6:
   PALET[33]←BYTE(SEAUM);
   PALET[34]←BYTE(SEAAUM);
   COR1←BYTE(SEA);
   COR2←BYTE(SEAM);
   FIM_CASO;
   CASO 1, 3, 5, 7:
   PALET[33]←BYTE(SEAUM);
   PALET[34]←BYTE(SEAAUM);
   COR1←BYTE(SEA);
   COR2←BYTE(SEAM);
   FIM_CASO;
   FIM_SELECT;
   ESCREVE_VIDEO(WT,WH,80,480,PALET,COR1,COR2,8);
   FIM_PARA;
   FIM_PARA;
   FIM_PROC;

*FUNCOES
   COR FUN_COR_MEDIA(COR COR1, COR COR2) LOCAL;
   COR RESULTADO;
   RESULTADO.B←BYTE((COR1.B+COR2.B)/2);
   RESULTADO.G←BYTE((COR1.G+COR2.G)/2);
   RESULTADO.R←BYTE((COR1.R+COR2.R)/2);
   FUN_COR_MEDIA←RESULTADO;
   FIM_FUN;

*FUNCOES
   COR FUN_COR_SOMBRIA(COR COR1) LOCAL;
   COR RESULTADO;
   RESULTADO.B←BYTE(COR1.B/2);
   RESULTADO.G←BYTE(COR1.G/2);
   RESULTADO.R←BYTE(COR1.R/2);
   FUN_COR_SOMBRIA←RESULTADO;
   FIM_FUN;

*FUNCOES
   COR FUN_COR_BRILHANTE(COR COR1) LOCAL;
   COR RESULTADO;
   RESULTADO.B←BYTE((COR1.B*3)/2);
   RESULTADO.G←BYTE((COR1.G*3)/2);
   RESULTADO.R←BYTE((COR1.R*3)/2);
   FUN_COR_BRILHANTE←RESULTADO;
   FIM_FUN;

FIM_PROGRAMA;
```

This code will create a 40-line video mode with a palette of 64 colors. The palette will be initialized with the following colors:

* Black
* Dark gray
* Light gray
* White
* Red
* Green
* Blue

The video mode will be filled with a pattern of stripes, with each stripe being a different color. The stripes will be arranged in a grid, with 8 stripes per row and 40 rows per screen.

The code uses a number of procedures and functions to initialize the video mode, create the palette, and fill the video mode with the stripes. The following is a brief description of each procedure and function:

* **INCIALIZA_CORES()**: This procedure initializes the video mode and sets the cursor color and shape.
* **INCIALIZA_VETOR()**: This procedure initializes the array `V` with the values needed to create the stripes.
* **Efetua_Video_40_Linhas()**: This procedure fills the video mode with the stripes.
* **FUN_EXTRACT_COR()**: This function returns the color that is halfway between two given colors.
* **FUN_MEDIA_COR()**: This function returns the average color of two given colors.
* **FUN_SOMBRIA_COR()**: This function returns a darker version of a given color.
* **FUN_BRILHANTE_COR()**: This function returns a brighter version of a given color.

The code also uses a number of constants to define the colors used in the palette and the stripes. The following is a list of these constants:

* **PRETO**: The color black.
* **BRANCO**: The color white.
* **CINZA_ESCURO**: The color dark gray.
* **CINZA_CLARO**: The color light gray.
* **VERMELHO**: The color red.
* **VERDE**: The color green.
* **AZUL**: The color blue.

The code also uses a number of variables to store the values needed to create the stripes. The following is a list of these variables:

* **IA**: An integer variable used to loop through the rows of the video mode.
* **JA**: An integer variable used to loop through the columns of the video mode.
* **SEA**: A real variable used to store the value of the red component of the color used for the current stripe.
* **SEAUM**: A real variable used to store the value of the green component of the color used for the current stripe.
* **SEAM**: A real variable used to store the value of the blue component of the color used for the current stripe.
* **SEAAUM**: A real variable used to store the value of the alpha component of the color used for the current stripe.
* **V**: An array of bytes used to store the values needed to create the stripes.
* **ENTRA**: A color variable used to store the color of the first stripe.
* **VERDE**: A color variable used to store the color of the second stripe.
* **VERMELHO**: A color variable used to store the color of the third stripe.
* **AZUL**: A color variable used to store the color of the fourth stripe.