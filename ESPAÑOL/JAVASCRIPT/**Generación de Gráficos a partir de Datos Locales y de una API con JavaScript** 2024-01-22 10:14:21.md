```javascript
// Función para crear un objeto de opciones para el gráfico.
function crearOpcionesGrafico(datos, etiquetas, titulo) {
  return {
    type: 'line',
    data: {
      labels: etiquetas,
      datasets: [{
        label: titulo,
        data: datos,
        backgroundColor: 'rgba(75, 192, 192, 0.2)',
        borderColor: 'rgba(75, 192, 192, 1)',
        borderWidth: 1
      }]
    },
    options: {
      scales: {
        yAxes: [{
          ticks: {
            beginAtZero: true
          }
        }]
      }
    }
  };
}

// Función para generar un gráfico.
function generarGrafico(lienzo, opciones) {
  const ctx = lienzo.getContext('2d');
  return new Chart(ctx, opciones);
}

// Función para obtener datos de una API.
function obtenerDatosAPI(url) {
  return fetch(url)
    .then(res => res.json());
}

// Función para crear un gráfico a partir de datos de una API.
function crearGraficoDesdeAPI(lienzo, url) {
  obtenerDatosAPI(url)
    .then(datos => {
      const etiquetas = Object.keys(datos);
      const datosGrafico = Object.values(datos);
      const opciones = crearOpcionesGrafico(datosGrafico, etiquetas, 'Título del gráfico');
      generarGrafico(lienzo, opciones);
    });
}

// Crear un gráfico a partir de datos locales.
const datosLocales = [10, 20, 30, 40, 50];
const etiquetasLocales = ['Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo'];
const lienzoLocal = document.getElementById('grafico-local');
const opcionesLocales = crearOpcionesGrafico(datosLocales, etiquetasLocales, 'Título del gráfico local');
const graficoLocal = generarGrafico(lienzoLocal, opcionesLocales);

// Crear un gráfico a partir de datos de una API.
const urlAPI = 'https://api.example.com/datos';
const lienzoAPI = document.getElementById('grafico-api');
crearGraficoDesdeAPI(lienzoAPI, urlAPI);
```

Explicación del código:

* La función `crearOpcionesGrafico` crea un objeto de opciones para el gráfico. Este objeto especifica el tipo de gráfico, los datos del gráfico y las opciones de visualización.
* La función `generarGrafico` genera un gráfico utilizando el objeto de opciones proporcionado.
* La función `obtenerDatosAPI` obtiene datos de una API.
* La función `crearGraficoDesdeAPI` crea un gráfico a partir de datos obtenidos de una API.
* El código crea dos gráficos: uno utilizando datos locales y otro utilizando datos obtenidos de una API.