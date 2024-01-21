```forth

: Crear-Lista ( tam ) \ Crea una lista de 'tam' elementos vacíos
   tam 0 do
      0
   loop
   cr cr ;

: Imprimir-Lista ( lista ) \ Imprime los elementos de una lista separados por espacios
   lista begin
      dup @
      .
      dup @
      0=
   while
      drop drop
      cr
   repeat
   cr cr ;

: Insertar-Elemento ( lista elemento ) \ Inserta 'elemento' en la cabeza de la lista
   swap
   over @ swap
   rot @ @ swap
   , ;

: Buscar-Elemento ( lista elemento ) \ Busca 'elemento' en la lista y devuelve su índice o -1 si no lo encuentra
   begin
      dup @
      dup @
      0=
   while
      drop drop
      -1
   repeat
   drop ;

: Eliminar-Elemento ( lista elemento ) \ Elimina 'elemento' de la lista si existe
   begin
      dup @
      dup @
      0=
   while
      over @ @
      =
   while
      swap
      drop
      rot @ @
      rot 1+ @ swap
      swap @
      ,
      rot @ @
      - rot @ @
      swap ,
   repeat
   drop ;

: Ordenar-Lista ( lista ) \ Ordena la lista en orden ascendente
   begin
      0
      [
         dup @
         1+ until
         dup @
         drop
      ]
      each
      begin
         0
         [
            dup @
            1+ until
            dup @
            drop
         ]
         each
         begin
            dup @
            over @
            >
         while
            swap
            over @
            over @
            swap
            ,
            rot @ @
            over @
            over @
            1- swap
            ,
         repeat
      ]
      each
   ]
   while
   drop ;

: Invertir-Lista ( lista ) \ Invierte el orden de los elementos de la lista
   begin
      dup @
      dup @
      0=
   while
      drop drop
      begin
         dup @
         dup @
         0=
      while
         drop drop
         over
         swap
      repeat
      drop
   ]
   while
   drop ;

: Fibonacci ( n ) \ Calcula el n-ésimo número de Fibonacci
   begin
      dup @ 0=
   while
      drop 1
   repeat
   dup @ 1=
   while
      drop 1
   repeat
   begin
      dup @
      dup @
      -
      drop
      over @
      over @
      +
      rot +
   ]
   while
   drop ;

: Factorial ( n ) \ Calcula el factorial de 'n'
   begin
      dup @ 1=
   while
      drop 1
   repeat
   begin
      dup @
      over @
      *
   ]
   while
   drop ;

```