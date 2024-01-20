```smalltalk
** Clase Punto **

Punto >> inicializar: x y
    | puntoTabla |
    puntoTabla := Array new: 2.
    puntoTabla at: 1 put: x.
    puntoTabla at: 2 put: y.
    super inicializar: puntoTabla.

Punto >> x
    self at: 1.

Punto >> y
    self at: 2.

Punto >> trasladar: dx dy
    | nuevoPunto |
    nuevoPunto := self copy.
    nuevoPunto at: 1 put: (self x + dx).
    nuevoPunto at: 2 put: (self y + dy).
    nuevoPunto.

Punto >> imprimir
    Transcript show: (String streamContents: [ :s |
        s nextPutAll: 'Punto: (';
        s nextPutAll: (self x asString);
        s nextPutAll: ', ';
        s nextPutAll: (self y asString);
        s nextPutAll: ').']);

** Clase Línea **

Línea >> inicializar: puntoInicial puntoFinal
    | líneaTabla |
    líneaTabla := Array new: 2.
    líneaTabla at: 1 put: puntoInicial.
    líneaTabla at: 2 put: puntoFinal.
    super inicializar: líneaTabla.

Línea >> puntoInicial
    self at: 1.

Línea >> puntoFinal
    self at: 2.

Línea >> trasladar: dx dy
    | nuevoPuntoInicial nuevoPuntoFinal |
    nuevoPuntoInicial := (self puntoInicial trasladar: dx dy).
    nuevoPuntoFinal := (self puntoFinal trasladar: dx dy).
    self actualizar: nuevoPuntoInicial :nuevoPuntoFinal.

Línea >> actualizar: puntoInicial :puntoFinal
    | líneaTabla |
    líneaTabla := self at: Shape.
    líneaTabla at: 1 put: puntoInicial.
    líneaTabla at: 2 put: puntoFinal.

Línea >> imprimir
    Transcript show: (String streamContents: [ :s |
        s nextPutAll: 'Línea: (';
        s nextPutAll: (self puntoInicial x asString);
        s nextPutAll: ', ';
        s nextPutAll: (self puntoInicial y asString);
        s nextPutAll: ') - (';
        s nextPutAll: (self puntoFinal x asString);
        s nextPutAll: ', ';
        s nextPutAll: (self puntoFinal y asString);
        s nextPutAll: ').']);

** Clase Rectángulo **

Rectángulo >> inicializar: esquinaSuperiorIzquierda esquinaInferiorDerecha
    | rectánguloTabla |
    rectánguloTabla := Array new: 2.
    rectánguloTabla at: 1 put: esquinaSuperiorIzquierda.
    rectánguloTabla at: 2 put: esquinaInferiorDerecha.
    super inicializar: rectánguloTabla.

Rectángulo >> esquinaSuperiorIzquierda
    self at: 1.

Rectángulo >> esquinaInferiorDerecha
    self at: 2.

Rectángulo >> ancho
    self esquinaInferiorDerecha x - self esquinaSuperiorIzquierda x.

Rectángulo >> alto
    self esquinaInferiorDerecha y - self esquinaSuperiorIzquierda y.

Rectángulo >> trasladar: dx dy
    | nuevoPunto esquinaSuperiorIzquierda esquinaInferiorDerecha |
    nuevoPunto := Punto trasladar: dx dy.
    esquinaSuperiorIzquierda := (self esquinaSuperiorIzquierda trasladar: dx dy).
    esquinaInferiorDerecha := (self esquinaInferiorDerecha trasladar: dx dy).
    self actualizar: esquinaSuperiorIzquierda :esquinaInferiorDerecha.

Rectángulo >> actualizar: esquinaSuperiorIzquierda :esquinaInferiorDerecha
    | rectánguloTabla |
    rectánguloTabla := self at: Shape.
    rectánguloTabla at: 1 put: esquinaSuperiorIzquierda.
    rectánguloTabla at: 2 put: esquinaInferiorDerecha.

Rectángulo >> imprimir
    Transcript show: (String streamContents: [ :s |
        s nextPutAll: 'Rectángulo: (';
        s nextPutAll: (self esquinaSuperiorIzquierda x asString);
        s nextPutAll: ', ';
        s nextPutAll: (self esquinaSuperiorIzquierda y asString);
        s nextPutAll: ') - (';
        s nextPutAll: (self esquinaInferiorDerecha x asString);
        s nextPutAll: ', ';
        s nextPutAll: (self esquinaInferiorDerecha y asString);
        s nextPutAll: ').']);

** Clase Círculo **

Círculo >> inicializar: centro radio
    | círculoTabla |
    círculoTabla := Array new: 2.
    círculoTabla at: 1 put: centro.
    círculoTabla at: 2 put: radio.
    super inicializar: círculoTabla.

Círculo >> centro
    self at: 1.

Círculo >> radio
    self at: 2.

Círculo >> trasladar: dx dy
    | nuevoPunto |
    nuevoPunto := (self centro trasladar: dx dy).
    self actualizar: nuevoPunto.

Círculo >> actualizar: centro
    | círculoTabla |
    círculoTabla := self at: Shape.
    círculoTabla at: 1 put: centro.

Círculo >> imprimir
    Transcript show: (String streamContents: [ :s |
        s nextPutAll: 'Círculo: (';
        s nextPutAll: (self centro x asString);
        s nextPutAll: ', ';
        s nextPutAll: (self centro y asString);
        s nextPutAll: ') - ';
        s nextPutAll: (self radio asString);
        s nextPutAll: ' unidades de radio.']);

** Clase Imagen **

Imagen >> inicializar: nombreArchivo
    | imagenTabla |
    imagenTabla := Array new: 1.
    imagenTabla at: 1 put: nombreArchivo.
    super inicializar: imagenTabla.

Imagen >> dibujar: ventana rectángulo
    | bitmap datosImagen |
    bitmap := window bitmapFor: (self at: Image).
    datosImagen := bitmap data.
    datosImagen replaceFrom: 1 to: datosImagen size with: (String streamContents:
        [:s | s nextPutAll: (self at: Image) asByteArray]).
    bitmap lock: false.

Imagen >> imprimir
    Transcript show: (String streamContents:
        [:s | s nextPutAll: 'Imagen: '; s nextPutAll: (self at: Image asString); s nextPutAll: '.']);

** Clase Lienzo **

Lienzo >> inicializar
    | figuras |
    figuras := Array new.
    super inicializar: figuras.

Lienzo >> limpiar
    | figuras |
    figuras := self at: Shape.
    figuras do: [:cada | cada eliminar].

Lienzo >> agregar: unaFigura
    | figuras |
    figuras := self at: Shape.
    figuras add: unaFigura.

Lienzo >> eliminar: unaFigura
    | figuras |
    figuras := self at: Shape.
    figuras remove: unaFigura.

Lienzo >> imprimir
    | figuras |
    figuras := self at: Shape.
    Transcript show: 'Figuras en el lienzo: '.
    figuras do: [:cada | cada imprimir].

** Clase Ventana **

Ventana >> inicializar: tamaño título
    | ventana |
    ventana := Window new.
    ventana size: tamaño.
    ventana title: título.
    ventana menubar: Menubar new.
    ventana client: [Lienzo new].
    super inicializar: ventana.

Ventana >> lienzo
    self client as: Lienzo.

Ventana >> menuPrincipal
    self menubar.

Ventana >> agregarFigura: unaFigura
    self lienzo agregar: unaFigura.

Ventana >> imprimirFiguras
    self lienzo imprimir.

** Clase Aplicación **

Aplicación >> inicializar
    | ventana |
    ventana := Ventana new.
    ventana lienzo agregar: Punto nuevo: 100 @ 100.
    ventana lienzo agregar: Rectángulo nuevo: 100 @ 100 esquinaInferiorDerecha: 200 @ 200.
    ventana lienzo agregar: Círculo nuevo: 160 @ 120 radio: 60.
    ventana lienzo agregar: Línea nuevo: 150 @ 150 puntoFinal: 220 @ 220.
    ventana lienzo agregar: Imagen nueva: 'icono.png'.
    ventana imprimirFiguras.
    super inicializar: ventana.

** Clase Menubar **

Menubar >> inicializar
    super inicializar: MenuBar new.
    self add: (Menu nuevo: 'Figura') add: (MenuItem nuevo: 'Punto') add: (MenuItem nuevo: 'Línea') add: (MenuItem nuevo: 'Rectángulo') add: (MenuItem nuevo: 'Círculo') add: (MenuItem nuevo: 'Imagen').
    self add: (Menu nuevo: 'Edición') add: (MenuItem nuevo: 'Limpiar'.
    self add: (Menu nuevo: 'Ayuda') add: (MenuItem nuevo: 'Acerca de...').

Menubar >> menúFigura
    self at: 1.

Menubar >> menúEdición
    self at: 2.

Menubar >> menúAyuda
    self at: 3.

Menubar >> seleccionarItem: unItem
    | menú |
    menú := self selectItem: unItem.
    menú identifier = 'Limpiar' ifTrue: [self lienzo limpiar].
    menú identifier = 'Punto' ifTrue: [self lienzo agregar: Punto nuevo].
    menú identifier = 'Línea' ifTrue: [self lienzo agregar: Línea nueva].
    menú identifier = 'Rectángulo' ifTrue: [self lienzo agregar: Rectángulo nuevo].
    menú identifier = 'Círculo' ifTrue: [self lienzo agregar: Círculo nuevo].
    menú identifier = 'Imagen' ifTrue: [self lienzo agregar: Imagen nueva].
    menú identifier = 'Acerca de...' ifTrue: [Transcript show: 'Aplicación de dibujo - Copyright 2023'].

** Clase Shape **

Shape >> inicializar: tabla
    | figuras |
    figuras := self at: Shape.
    figuras add: tabla.
    super inicializar.

Shape >> eliminar
    | figuras |
    figuras := self at: Shape.
    figuras remove: self.

Shape >> imprimir
    Transcript show: 'Figura abstracta'.

** Clase Menu **

Menu >> nuevo: título
    super inicializar: MenuBarMenu nuevo.
    self title: título.

** Clase MenuItem **

MenuItem >> nuevo: título
    super inicializar: MenuBarMenuItem nuevo.
    self title: título.

** Clase VentanaAcercaDe **

VentanaAcercaDe >> inicializar
    | ventana |
    ventana := Window new.
    ventana size: 200 @ 100.
    ventana title: 'Acerca de...'.
    super inicializar: ventana.

VentanaAcercaDe >> botónAceptar
    | botón |
    botón := (self client as: Panel) add: Button nuevo.
    botón label: 'Aceptar'.
    botón onAction: [:e | self close].
```

**Explicación del código:**

- **Clase Shape:** Es una clase abstracta que define las operaciones básicas de una figura geométrica. Incluye un método `inicializar` que recibe una tabla con las propiedades de la figura, un método `eliminar` para eliminar la figura del lienzo y un método `imprimir` para mostrar la información de la figura.

- **Clase Punto:** Es una clase que define un punto en el espacio. Incluye un método `trasladar` para mover el punto a una nueva posición.

- **Clase Línea:** Es una clase que define una línea entre dos puntos. Incluye un método `trasladar` para mover la línea a una nueva posición.

- **Clase Rectángulo:** Es una clase que define un rectángulo en el espacio. Incluye un método `trasladar` para mover el rectángulo a una nueva posición.

- **Clase Círculo:** Es una clase que define un círculo en el espacio. Incluye un método `trasladar` para mover el círculo a una nueva posición.

- **Clase Imagen:** Es una clase que define una imagen en el espacio. Incluye un método `dibujar` para dibujar la imagen en un lienzo.

- **Clase Lienzo:** Es una clase que define un lienzo donde se dibujan las figuras. Incluye un método `limpiar` para eliminar todas las figuras del lienzo, un método `agregar` para añadir una figura al lienzo y un método `eliminar` para eliminar una figura del lienzo.

- **Clase Ventana:** Es una clase que define una ventana donde se muestra el lienzo. Incluye un método `inicializar` que recibe el tamaño y el título de la ventana, un método `lienzo` para obtener el lienzo de la ventana, un método `menuPrincipal` para obtener el menú principal de la ventana y un método `agregarFigura` para añadir una figura al lienzo.

- **Clase Aplicación:** Es una clase que define la aplicación principal. Incluye un método `inicializar` que recibe una ventana, un método `ventana` para obtener la ventana de la aplicación y un método `imprimirFiguras` para mostrar la información de las figuras en el lienzo.

- **Clase Menubar:** Es una clase que define el menú principal de la aplicación. Incluye un método `inicializar` que recibe un menú, un método `menúFigura` para obtener el menú de figuras, un método `menúEdición` para obtener el menú de edición y un método `menúAyuda` para obtener el menú de ayuda.

- **Clase MenuItem:** Es una clase que define un elemento del menú. Incluye un método `nuevo` que recibe el título del elemento, un método `title` para obtener el título del elemento y un método `identifier` para obtener el identificador del elemento.

- **Clase VentanaAcercaDe:** Es una clase que define una ventana que muestra información sobre la aplicación. Incluye un método `inicializar` que recibe el tamaño y el título de la ventana, un método `botónAceptar` para obtener el botón de aceptar de la ventana y un método `close` para cerrar la ventana.