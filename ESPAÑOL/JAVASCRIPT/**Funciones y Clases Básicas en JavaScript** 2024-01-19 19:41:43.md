```javascript
// Crear una función para saludar a alguien
function saludar(nombre) {
  console.log("Hola, " + nombre + "!");
}

// Crear una función para calcular el área de un triángulo
function calcularAreaTriangulo(base, altura) {
  return (base * altura) / 2;
}

// Crear una función para verificar si un número es par
function esPar(numero) {
  return numero % 2 === 0;
}

// Crear una función para encontrar el elemento más grande en una lista
function encontrarElementoMasGrande(lista) {
  let elementoMasGrande = lista[0];
  for (let i = 1; i < lista.length; i++) {
    if (lista[i] > elementoMasGrande) {
      elementoMasGrande = lista[i];
    }
  }
  return elementoMasGrande;
}

// Crear una función para ordenar una lista de números en orden ascendente
function ordenarListaNumeros(lista) {
  lista.sort(function(a, b) {
    return a - b;
  });
  return lista;
}

// Crear una función para crear un objeto con propiedades dinámicas
function crearObjeto(propiedades) {
  let objeto = {};
  for (let propiedad in propiedades) {
    objeto[propiedad] = propiedades[propiedad];
  }
  return objeto;
}

// Crear una función para hacer una petición HTTP GET a una URL
function hacerPeticionHTTPGET(url) {
  return new Promise(function(resolve, reject) {
    let xhr = new XMLHttpRequest();
    xhr.open("GET", url);
    xhr.onload = function() {
      if (xhr.status === 200) {
        resolve(xhr.responseText);
      } else {
        reject(xhr.statusText);
      }
    };
    xhr.onerror = function() {
      reject(xhr.statusText);
    };
    xhr.send();
  });
}

// Crear una función para convertir una cadena de texto en un objeto JSON
function convertirCadenaJSON(cadena) {
  return JSON.parse(cadena);
}

// Crear una función para convertir un objeto JSON en una cadena de texto
function convertirObjetoJSON(objeto) {
  return JSON.stringify(objeto);
}

// Crear una función para crear un elemento HTML
function crearElementoHTML(tipo, contenido) {
  let elemento = document.createElement(tipo);
  elemento.innerHTML = contenido;
  return elemento;
}

// Crear una función para añadir un evento a un elemento HTML
function añadirEvento(elemento, evento, funcion) {
  elemento.addEventListener(evento, funcion);
}

// Crear una función para eliminar un evento de un elemento HTML
function eliminarEvento(elemento, evento, funcion) {
  elemento.removeEventListener(evento, funcion);
}

// Crear una función para obtener el valor de un elemento de formulario
function obtenerValorFormulario(elemento) {
  return elemento.value;
}

// Crear una función para establecer el valor de un elemento de formulario
function establecerValorFormulario(elemento, valor) {
  elemento.value = valor;
}

// Crear una función para mostrar un elemento HTML
function mostrarElemento(elemento) {
  elemento.style.display = "block";
}

// Crear una función para ocultar un elemento HTML
function ocultarElemento(elemento) {
  elemento.style.display = "none";
}

// Crear una función para crear una cookie
function crearCookie(nombre, valor, dias) {
  let fechaExpiracion = new Date();
  fechaExpiracion.setDate(fechaExpiracion.getDate() + dias);
  document.cookie = nombre + "=" + valor + "; expires=" + fechaExpiracion.toUTCString();
}

// Crear una función para obtener el valor de una cookie
function obtenerValorCookie(nombre) {
  let nombreCookie = nombre + "=";
  let cookies = document.cookie.split(";");
  for (let i = 0; i < cookies.length; i++) {
    let cookie = cookies[i];
    while (cookie.charAt(0) === " ") {
      cookie = cookie.substring(1);
    }
    if (cookie.indexOf(nombreCookie) === 0) {
      return cookie.substring(nombreCookie.length, cookie.length);
    }
  }
  return "";
}

// Crear una función para eliminar una cookie
function eliminarCookie(nombre) {
  document.cookie = nombre + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC";
}

// Crear una función para crear una sesión de almacenamiento local
function crearSesionAlmacenamientoLocal(nombre, valor) {
  localStorage.setItem(nombre, valor);
}

// Crear una función para obtener el valor de una sesión de almacenamiento local
function obtenerValorSesionAlmacenamientoLocal(nombre) {
  return localStorage.getItem(nombre);
}

// Crear una función para eliminar una sesión de almacenamiento local
function eliminarSesionAlmacenamientoLocal(nombre) {
  localStorage.removeItem(nombre);
}

// Crear una función para crear una sesión de almacenamiento de sesión
function crearSesionAlmacenamientoSesion(nombre, valor) {
  sessionStorage.setItem(nombre, valor);
}

// Crear una función para obtener el valor de una sesión de almacenamiento de sesión
function obtenerValorSesionAlmacenamientoSesion(nombre) {
  return sessionStorage.getItem(nombre);
}

// Crear una función para eliminar una sesión de almacenamiento de sesión
function eliminarSesionAlmacenamientoSesion(nombre) {
  sessionStorage.removeItem(nombre);
}

// Crear una función para crear una función de retrollamada
function crearFuncionRetrollamada(funcion) {
  return function() {
    funcion();
  };
}

// Crear una función para crear un cierre
function crearCierre(valor) {
  return function() {
    return valor;
  };
}

// Crear una función para crear un generador
function crearGenerador() {
  let contador = 0;
  return function() {
    return contador++;
  };
}

// Crear una función para crear un proxy
function crearProxy(objeto) {
  return new Proxy(objeto, {
    get: function(target, propiedad) {
      console.log("La propiedad" + propiedad + "ha sido accedida.");
      return target[propiedad];
    },
    set: function(target, propiedad, valor) {
      console.log("La propiedad" + propiedad + "ha sido cambiada a" + valor + ".");
      target[propiedad] = valor;
      return true;
    }
  });
}

// Crear una función para crear una clase
function crearClase(nombre, constructor, metodos