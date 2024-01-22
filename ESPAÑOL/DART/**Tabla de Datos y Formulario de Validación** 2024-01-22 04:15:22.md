```dart
// Obtener los elementos del DOM por su ID, es una función utilizada varias veces en el código.
dynamic getById(String id) {
  return document.querySelector('#$id');
}

// Clase para gestionar la tabla de datos.
class Tabla {
  // Elemento que contiene la tabla.
  Element tableElement;

  // Cabezales de la tabla.
  List<String> cabeceras;

  // Datos de la tabla.
  List<List<String>> datos;

  // Constructor de la clase.
  Tabla(this.tableElement, this.cabeceras, this.datos) {
    crearTabla();
  }

  // Crea la tabla en el DOM.
  void crearTabla() {
    // Crear el elemento de la tabla.
    tableElement.createFragment().children.add(Element.tag('table'));

    // Crear el elemento del encabezado de la tabla.
    var thead = Element.tag('thead');

    // Crear una fila para los encabezados.
    var tr = Element.tag('tr');

    // Recorrer los encabezados y crear una celda para cada uno.
    for (var i = 0; i < cabeceras.length; i++) {
      tr.createFragment().children.add(Element.tag('th')..text = cabeceras[i]);
    }

    // Agregar la fila de encabezados al encabezado de la tabla.
    thead.append(tr);

    // Crear el elemento del cuerpo de la tabla.
    var tbody = Element.tag('tbody');

    // Recorrer los datos y crear una fila para cada uno.
    for (var i = 0; i < datos.length; i++) {
      tr = Element.tag('tr');

      // Recorrer los datos de la fila y crear una celda para cada uno.
      for (var j = 0; j < datos[i].length; j++) {
        tr.createFragment().children.add(Element.tag('td')..text = datos[i][j]);
      }

      // Agregar la fila al cuerpo de la tabla.
      tbody.append(tr);
    }

    // Agregar el encabezado y el cuerpo de la tabla al elemento de la tabla.
    tableElement.querySelector('table')!.append(thead, tbody);
  }
}

// Función para obtener los datos de un formulario.
Map<String, String> obtenerDatosFormulario(Element form) {
  var datos = <String, String>{};

  // Recorrer los elementos del formulario y obtener sus valores.
  for (var input in form.children) {
    if (input is Element && input.tagName == 'INPUT') {
      datos[input.name!] = input.value!;
    }
  }

  return datos;
}

// Función para crear un elemento del DOM.
Element crearElemento(String etiqueta, [Map<String, String>? atributos]) {
  var elemento = Element.tag(etiqueta);

  // Asignar los atributos al elemento.
  if (atributos != null) {
    for (var atributo in atributos.keys) {
      elemento.setAttribute(atributo, atributos[atributo]!);
    }
  }

  return elemento;
}

// Función para agregar un mensaje de error a un elemento.
void agregarMensajeError(Element elemento, String mensaje) {
  // Crear un elemento para el mensaje de error.
  var mensajeError = crearElemento('div', {'class': 'error-message'});
  mensajeError.text = mensaje;

  // Agregar el mensaje de error al elemento.
  elemento.append(mensajeError);
}

// Función para validar un formulario.
bool validarFormulario(Element form) {
  // Obtener los datos del formulario.
  var datos = obtenerDatosFormulario(form);

  // Bandera para indicar si el formulario es válido.
  var valido = true;

  // Recorrer los datos del formulario y validar cada campo.
  for (var dato in datos.keys) {
    // Comprobar si el campo está vacío.
    if (datos[dato]!.isEmpty) {
      agregarMensajeError(getById(dato), 'Este campo no puede estar vacío.');
      valido = false;
    }
  }

  return valido;
}

// Función para enviar un formulario.
void enviarFormulario(Element form) {
  // Validar el formulario.
  if (!validarFormulario(form)) {
    return;
  }

  // Obtener los datos del formulario.
  var datos = obtenerDatosFormulario(form);

  // Enviar los datos del formulario a un servidor o realizar otra acción.
  // En este ejemplo, solo los imprimimos en la consola.
  print(datos);
}

// Crear la tabla de datos.
var tabla = Tabla(getById('tabla'), ['Nombre', 'Apellido', 'Edad'], [
  ['Juan', 'García', '23'],
  ['María', 'López', '25'],
  ['Pedro', 'Sánchez', '28'],
]);

// Crear el formulario.
var form = crearElemento('form', {'id': 'formulario'});

// Crear los campos del formulario.
var nombre = crearElemento('input', {'type': 'text', 'name': 'nombre'});
var apellido = crearElemento('input', {'type': 'text', 'name': 'apellido'});
var edad = crearElemento('input', {'type': 'number', 'name': 'edad'});

// Crear el botón de envío.
var enviar = crearElemento('input', {'type': 'submit', 'value': 'Enviar'});

// Agregar los campos del formulario al formulario.
form.append(crearElemento('label', {'for': 'nombre'})..text = 'Nombre:', nombre);
form.append(crearElemento('br'));
form.append(crearElemento('label', {'for': 'apellido'})..text = 'Apellido:', apellido);
form.append(crearElemento('br'));
form.append(crearElemento('label', {'for': 'edad'})..text = 'Edad:', edad);
form.append(crearElemento('br'));
form.append(enviar);

// Agregar el formulario al DOM.
getById('formulario-contenedor').append(form);

// Agregar un evento al formulario para manejar su envío.
form.addEventListener('submit', (event) {
  event.preventDefault();
  enviarFormulario(form);
});
```

Este código crea una tabla de datos con tres columnas y tres filas, un formulario con tres campos de texto y un botón de envío, y añade el formulario al DOM. Cuando el usuario introduce datos en el formulario y hace clic en el botón de envío, el código valida los datos del formulario y los envía a un servidor o realiza otra acción (en este ejemplo, solo los imprime en la consola).