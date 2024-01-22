```javascript
// 1. Definir una función recursiva para calcular el factorial de un número

function factorial(numero) {
  if (numero === 0) {
    return 1;
  }
  return numero * factorial(numero - 1);
}

// 2. Definir una función para crear un objeto JSON a partir de un objeto JavaScript

function crearJSON(objeto) {
  return JSON.stringify(objeto);
}

// 3. Definir una función para convertir una cadena JSON a un objeto JavaScript

function convertirJSON(cadenaJSON) {
  return JSON.parse(cadenaJSON);
}

// 4. Definir una función para crear una promesa y resolverla después de un tiempo determinado

function crearPromesa(tiempoDeEspera) {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      resolve("¡Promesa resuelta!");
    }, tiempoDeEspera);
  });
}

// 5. Definir una función para crear una expresión regular y validar una dirección de correo electrónico

function validarEmail(email) {
  const regex = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
  return regex.test(email);
}

// 6. Definir una función para crear un evento personalizado y dispararlo

function crearEventoPersonalizado(nombreEvento) {
  const evento = new CustomEvent(nombreEvento, {
    detail: {
      mensaje: "Este es un evento personalizado",
    },
  });
  window.dispatchEvent(evento);
}

// 7. Definir una función para crear un elemento HTML dinámico y añadirlo al DOM

function crearElementoHTML(etiqueta, texto, claseCSS) {
  const elemento = document.createElement(etiqueta);
  elemento.textContent = texto;
  elemento.classList.add(claseCSS);
  document.body.appendChild(elemento);
}

// 8. Definir una función para realizar una petición HTTP GET a una URL determinada

function realizarPeticionGET(url) {
  return fetch(url, {
    method: "GET",
  }).then((response) => response.json());
}

// 9. Definir una función para realizar una petición HTTP POST a una URL determinada

function realizarPeticionPOST(url, datos) {
  return fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(datos),
  }).then((response) => response.json());
}

// 10. Definir una función para crear un gráfico utilizando la biblioteca Chart.js

function crearGrafico(contexto, datos) {
  const grafico = new Chart(contexto, {
    type: "bar",
    data: {
      labels: ["Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio"],
      datasets: [
        {
          label: "Datos",
          data: datos,
        },
      ],
    },
    options: {
      scales: {
        yAxes: [
          {
            ticks: {
              beginAtZero: true,
            },
          },
        ],
      },
    },
  });
}
```

**Explicación del código:**

1. La función `factorial` calcula el factorial de un número utilizando recursión.
2. La función `crearJSON` crea un objeto JSON a partir de un objeto JavaScript.
3. La función `convertirJSON` convierte una cadena JSON a un objeto JavaScript.
4. La función `crearPromesa` crea una promesa y la resuelve después de un tiempo determinado.
5. La función `validarEmail` valida una dirección de correo electrónico utilizando una expresión regular.
6. La función `crearEventoPersonalizado` crea un evento personalizado y lo dispara.
7. La función `crearElementoHTML` crea un elemento HTML dinámico y lo añade al DOM.
8. La función `realizarPeticionGET` realiza una petición HTTP GET a una URL determinada.
9. La función `realizarPeticionPOST` realiza una petición HTTP POST a una URL determinada.
10. La función `crearGrafico` crea un gráfico utilizando la biblioteca Chart.js.