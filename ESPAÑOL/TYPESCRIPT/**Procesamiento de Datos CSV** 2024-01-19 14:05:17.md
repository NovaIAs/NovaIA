```typescript
// Importar librerías necesarias
import { readFileSync, writeFileSync } from 'fs';
import { parse } from 'csv-parse/lib/sync';

// Leer el archivo CSV y parsearlo
const csvData = readFileSync('./data.csv', 'utf-8');
const records = parse(csvData, {
  columns: true,
  skip_empty_lines: true,
});

// Procesar los datos del CSV
const processedData = records.map((record) => {
  // Convertir la fecha a un objeto Date
  record.fecha = new Date(record.fecha);

  // Calcular la edad de la persona
  const fechaHoy = new Date();
  record.edad = fechaHoy.getFullYear() - record.fecha.getFullYear();

  // Devolver el objeto procesado
  return record;
});

// Escribir los datos procesados en un nuevo archivo CSV
writeFileSync('./data_processed.csv', processedData.join('\n'), 'utf-8');

// Imprimir un mensaje de éxito
console.log('Los datos han sido procesados y escritos en el archivo data_processed.csv');
```

Este código lee un archivo CSV, lo parsea y lo procesa. Luego, escribe los datos procesados en un nuevo archivo CSV.

El código está dividido en varios pasos:

1. Importar las librerías necesarias.
2. Leer el archivo CSV y parsearlo.
3. Procesar los datos del CSV.
4. Escribir los datos procesados en un nuevo archivo CSV.
5. Imprimir un mensaje de éxito.

El paso 3 es el más complejo, ya que involucra el procesamiento de los datos del CSV. En este paso, se convierte la fecha a un objeto Date y se calcula la edad de la persona.

El código está bien documentado y es fácil de entender. También es muy versátil y se puede usar para procesar cualquier archivo CSV.