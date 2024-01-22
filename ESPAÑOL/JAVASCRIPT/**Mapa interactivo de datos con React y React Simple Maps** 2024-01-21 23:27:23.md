```javascript
// Importa el marco de trabajo y paquetes necesarios
import React, { useState, useEffect, useRef } from 'react';
import {
  ComposableMap,
  Geographies,
  Geography,
  Marker,
  Annotation,
  ZoomableGroup,
  Line
} from "react-simple-maps";
import { scaleLinear } from "d3-scale";
import { csvParse } from 'd3-dsv';
import  {interpolateDark}  from "d3-scale-chromatic";
import LinearGradient from 'react-linear-gradient';

// Define el componente del mapa
function Mapa() {
  // Define el estado del mapa
  const [datos, setDatos] = useState([]);
  const [tooltip, setTooltip] = useState("");
  const [selectedCountry, setSelectedCountry] = useState(null);
  // Carga los datos del mapa
  useEffect(() => {
    (async () => {
      const datosCargados = await d3.csv( "./datos/mapa.csv");
      setDatos(datosCargados);
    })()
  }, []);

  // Define la escala de colores para los países
  const escalaColores = scaleLinear()
  .domain([0, 100])
  .range(["#fff", "#000"]);

  // Define la referencia al elemento del mapa
  const ref = useRef(null);

  // Maneja el evento de paso de ratón sobre un país
  const handleMouseOver = (geografia) => {
    let info = datos.filter(d => d.codigo === geografia.id)[0];
    setTooltip(`${info.pais} - ${info.valor}`);
  };

  // Maneja el evento de salida de ratón sobre un país
  const handleMouseOut = () => {
    setTooltip("");
  };

  // Maneja el evento de clic sobre un país
  const handleClick = (geografia) => {
    let info = datos.filter(d => d.codigo === geografia.id)[0];
    setSelectedCountry(info);
  };

  // Define la configuración del mapa
  const config = {
    width: 960,
    height: 500,
    geography: "https://unpkg.com/world-atlas/countries-50m.json",
    projection: "geoAzimuthalEquidistant",
    projectionConfig: {
      rotate: [0, 0, 0]
    }
  };
  const colorLine = interpolateDark(0,1)

  // Retorna el componente del mapa
  return (
    <div>
      <ComposableMap {...config} ref={ref}>
        <ZoomableGroup>
          <Geographies geography={config.geography}>
            {({ geographies }) => (
              <>
                {geographies.map((geografia) => (
                  <Geography
                    key={geografia.rsmKey}
                    geography={geografia}
                    onMouseOver={() => handleMouseOver(geografia)}
                    onMouseOut={() => handleMouseOut()}
                    onClick={() => handleClick(geografia)}
                    style={{
                      default: {
                        fill: escalaColores(
                          datos.filter(d => d.codigo === geografia.id)[0].valor
                        ),
                        stroke: "#fff",
                        strokeWidth: 0.5,
                        outline: "none"
                      },
                      hover: {
                        fill: "#ff0000",
                        stroke: "#fff",
                        strokeWidth: 1,
                        outline: "none"
                      },
                      pressed: {
                        fill: "#ff0000",
                        stroke: "#fff",
                        strokeWidth: 1,
                        outline: "none"
                      }
                    }}
                  />
                ))}
                {selectedCountry && (
                  <Annotation
                    subject={[selectedCountry.longitud, selectedCountry.latitud]}
                    dx={20}
                    dy={-40}
                    connectorProps={{
                      stroke: "#ff0000",
                      strokeWidth: 3,
                      strokeLinecap: "round"
                    }}
                  >
                    <rect width={120} height={40} fill="#ff0000" rx={10} ry={10} />
                    <text fontSize={12} dy={20} fill="#fff">
                      {selectedCountry.pais}
                    </text>
                    <text fontSize={12} dy={32} fill="#fff">
                      {selectedCountry.valor}
                    </text>
                  </Annotation>
                )}
              </>
            )}
          </Geographies>
          <Line
            from={{longitude: -60, latitude: 30}}
            to={{longitude: -20, latitude: 10}}
            stroke={colorLine(0.5)}
            strokeWidth={2}/>
        </ZoomableGroup>
      </ComposableMap>
      <div style={{ position: "absolute", left: "50%", transform: "translateX(-50%)"}}>
        {tooltip && <div style={{ padding: "10px", backgroundColor: "#ff0000", color: "#fff" }}>
          {tooltip}
        </div>}
      </div>
      <LinearGradient id="gradient" gradientDirection="90deg">
      <stop offset="20%" stopColor="#ff0000"></stop>
      <stop offset="100%" stopColor="#ffffff"></stop>
      </LinearGradient>
    </div>
  );
}

export default Mapa;
```

**Explicación del código:**

1. **Importación e inicialización de los datos del mapa:**
   - Se importa el marco de trabajo React y los paquetes necesarios para crear el mapa.
   - Se define el estado inicial del mapa, incluyendo los datos del mapa y la información de la herramienta emergente.
   - Se carga el archivo CSV que contiene los datos del mapa utilizando la función `d3.csv`.

2. **Definición de la escala de colores:**
   - Se define una escala de colores para los países del mapa utilizando `d3-scale`. La escala mapea los valores de los datos del mapa a una gama de colores.

3. **Definición del mapa:**
   - Se define el componente del mapa `Mapa` como una función que toma propiedades.
   - En el interior del componente, se utiliza el componente `ComposableMap` de la biblioteca `react-simple-maps` para crear un mapa.
   - Se definen la configuración del mapa, incluyendo el ancho, alto, la proyección y la ubicación del mapa.
   - Se utilizan los componentes `Geographies` y `Geography` de `react-simple-maps` para renderizar los países del mapa.
   - Se definen los estilos para los países, incluyendo el color de relleno, el ancho de borde y el estilo de cursor.
   - Se utilizan los eventos `onMouseOver`, `onMouseOut` y `onClick` para manejar los eventos de paso de ratón, salida de ratón y clic sobre los países.
   - Se utiliza el componente `Annotation` de `react-simple-maps` para mostrar la información de la herramienta emergente al pasar el ratón sobre un país.

4. **Manejo de eventos:**
   - Se definen las funciones `handleMouseOver`, `handleMouseOut` y `handleClick` para manejar los eventos de paso de ratón, salida de ratón y clic sobre los países.
   - Estas funciones actualizan el estado del mapa y muestran la información de la herramienta emergente.

5. **Renderizado del mapa:**
   - Se renderiza el mapa utilizando el componente `Mapa`.
   - El mapa se muestra en el navegador y los usuarios pueden interactuar con él.

Este código crea un mapa interactivo que permite a los usuarios explorar los datos del mapa y obtener más información sobre los países al pasar el ratón sobre ellos.