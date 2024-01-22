```go
// Importando las librerías necesarias
package main

import (
    "context"
    "fmt"
    "log"
    "time"

    "cloud.google.com/go/bigquery"
)

func main() {
    // Estableciendo el proyecto y el dataset
    projectID := "mi-proyecto-bigquery"
    datasetID := "mi-dataset"

    // Creando el cliente de BigQuery
    client, err := bigquery.NewClient(context.Background(), projectID)
    if err != nil {
        log.Fatalf("Error al crear el cliente de BigQuery: %v", err)
    }
    defer client.Close()

    // Creando el dataset
    dataset := client.Dataset(datasetID)
    if err := dataset.Create(context.Background(), &bigquery.DatasetMetadata{}); err != nil {
        log.Fatalf("Error al crear el dataset: %v", err)
    }

    // Creando una tabla vacía
    table := dataset.Table("mi-tabla")
    if err := table.Create(context.Background(), &bigquery.TableMetadata{}); err != nil {
        log.Fatalf("Error al crear la tabla: %v", err)
    }

    // Creando un esquema para la tabla
    schema := bigquery.Schema{
        {Name: "nombre", Type: bigquery.StringFieldType},
        {Name: "edad", Type: bigquery.IntegerFieldType},
        {Name: "fecha_nacimiento", Type: bigquery.DateFieldType},
    }

    // Actualizando el esquema de la tabla
    if err := table.Update(context.Background(), &bigquery.TableMetadataToUpdate{
        Schema: schema,
    }, bigquery.StandardTableOptions()); err != nil {
        log.Fatalf("Error al actualizar el esquema de la tabla: %v", err)
    }

    // Cargando datos en la tabla desde un slice de structs
    type Persona struct {
        Nombre           string
        Edad             int
        FechaNacimiento time.Time
    }

    data := []Persona{
        {"Juan", 30, time.Date(1990, 1, 1, 0, 0, 0, 0, time.UTC)},
        {"María", 25, time.Date(1995, 7, 15, 0, 0, 0, 0, time.UTC)},
        {"Pedro", 40, time.Date(1980, 12, 31, 0, 0, 0, 0, time.UTC)},
    }

    loader := client.Dataset(datasetID).Table(table.TableID).LoaderFrom(data)
    loader.WriteDisposition = bigquery.WriteTruncate
    if err := loader.Run(context.Background()); err != nil {
        log.Fatalf("Error al cargar los datos en la tabla: %v", err)
    }

    // Consultando los datos de la tabla
    query := client.Query("SELECT * FROM `mi-proyecto-bigquery.mi-dataset.mi-tabla`")
    it, err := query.Read(context.Background())
    if err != nil {
        log.Fatalf("Error al consultar los datos de la tabla: %v", err)
    }
    for {
        var row []bigquery.Value
        err := it.Next(&row)
        if err == iterator.Done {
            break
        }
        if err != nil {
            log.Fatalf("Error al iterar sobre los resultados de la consulta: %v", err)
        }
        fmt.Println(row)
    }

    // Eliminando la tabla
    if err := table.Delete(context.Background()); err != nil {
        log.Fatalf("Error al eliminar la tabla: %v", err)
    }

    // Eliminando el dataset
    if err := dataset.Delete(context.Background()); err != nil {
        log.Fatalf("Error al eliminar el dataset: %v", err)
    }
}
```

Este código realiza las siguientes operaciones:

1. Crea un nuevo proyecto y un nuevo dataset en BigQuery.
2. Crea una tabla vacía en el dataset.
3. Actualiza el esquema de la tabla para añadir tres columnas: nombre, edad y fecha_nacimiento.
4. Carga datos en la tabla desde un slice de structs.
5. Consulta los datos de la tabla.
6. Elimina la tabla y el dataset.

El código utiliza la librería oficial de BigQuery para Go, que proporciona una interfaz sencilla y fácil de usar para interactuar con BigQuery. El código está bien comentado y documentado, lo que hace que sea fácil de entender y modificar según las necesidades específicas.