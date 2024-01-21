```smalltalk
"Definir ventana principal"
VentanaPrincipal := UIManager newWindow.

"Definir título de la ventana"
VentanaPrincipal título: 'Ejemplo ventana en español'.

"Definir tamaño de la ventana"
VentanaPrincipal tamaño: {300, 300}.

"Definir contenido de la ventana"
VentanaPrincipal contenido: [

    "Crear botón"
    Boton := UIManager newButton.

    "Definir texto del botón"
    Boton texto: '¡Hola, mundo!'.

    "Definir acción del botón"
    Boton acción: [
        "Mostrar mensaje de alerta"
        UIManager newAlert título: '¡Hola, mundo!' mensaje: '¡Hola, mundo!'.
    ].

    "Añadir botón a la ventana"
    VentanaPrincipal addSubView: Botón.
].

"Mostrar ventana principal"
VentanaPrincipal open.
```

Explicación del código:

* **Definición de la ventana principal:** Se crea una instancia de `UIManager` llamada `VentanaPrincipal` utilizando el mensaje `newWindow`.
* **Definición del título de la ventana:** Se establece el título de la ventana utilizando el mensaje `título:`.
* **Definición del tamaño de la ventana:** Se establece el tamaño de la ventana utilizando el mensaje `tamaño:`.
* **Definición del contenido de la ventana:** Se añade contenido a la ventana utilizando el mensaje `contenido:`. El contenido es un bloque de código que define los elementos que se mostrarán en la ventana.
* **Creación de un botón:** Se crea una instancia de `UIManager` llamada `Boton` utilizando el mensaje `newButton`.
* **Definición del texto del botón:** Se establece el texto del botón utilizando el mensaje `texto:`.
* **Definición de la acción del botón:** Se establece la acción del botón utilizando el mensaje `acción:`. La acción es un bloque de código que se ejecutará cuando se haga clic en el botón.
* **Añadir botón a la ventana:** Se añade el botón a la ventana utilizando el mensaje `addSubView:`.
* **Mostrar ventana principal:** Se muestra la ventana principal utilizando el mensaje `open`.