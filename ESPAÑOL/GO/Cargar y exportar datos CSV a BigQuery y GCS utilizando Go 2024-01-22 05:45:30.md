```go
// Importamos las librerías necesarias
import (
    "bufio"
    "context"
    "encoding/csv"
    "fmt"
    "io"
    "log"
    "os"
    "strings"
    "time"

    "cloud.google.com/go/bigquery"
    "cloud.google.com/go/storage"
)

// Esta función carga los datos de un archivo CSV a una tabla de BigQuery.
func loadCSVToBigQuery(ctx context.Context, projectID string, datasetID string, tableID string, csvFile string) error {
    // Abrimos el archivo CSV
    file, err := os.Open(csvFile)
    if err != nil {
        return fmt.Errorf("Error al abrir el archivo CSV: %v", err)
    }
    defer file.Close()

    // Creamos un lector CSV
    reader := csv.NewReader(bufio.NewReader(file))

    // Creamos un cliente de BigQuery
    client, err := bigquery.NewClient(ctx, projectID)
    if err != nil {
        return fmt.Errorf("Error al crear el cliente de BigQuery: %v", err)
    }
    defer client.Close()

    // Creamos un dataset si no existe
    dataset := client.Dataset(datasetID)
    if err := dataset.Create(ctx, &bigquery.DatasetMetadata{}); err != nil {
        if !strings.Contains(err.Error(), "Dataset already exists") {
            return fmt.Errorf("Error al crear el dataset: %v", err)
        }
    }

    // Creamos una tabla si no existe
    table := dataset.Table(tableID)
    if err := table.Create(ctx, &bigquery.TableMetadata{}); err != nil {
        if !strings.Contains(err.Error(), "Table already exists") {
            return fmt.Errorf("Error al crear la tabla: %v", err)
        }
    }

    // Obtenemos el esquema de la tabla
    schema, err := table.Schema(ctx)
    if err != nil {
        return fmt.Errorf("Error al obtener el esquema de la tabla: %v", err)
    }

    // Creamos un escritor de filas
    writer := table.Inserter()

    // Leemos el archivo CSV y escribimos las filas en la tabla
    for {
        record, err := reader.Read()
        if err == io.EOF {
            break
        }
        if err != nil {
            return fmt.Errorf("Error al leer el archivo CSV: %v", err)
        }

        // Convertimos las filas en un mapa de valores
        row := make(map[string]bigquery.Value)
        for i, field := range schema {
            row[field.Name] = record[i]
        }

        // Escribimos la fila en la tabla
        if err := writer.Put(ctx, row); err != nil {
            return fmt.Errorf("Error al escribir la fila en la tabla: %v", err)
        }
    }

    // Esperamos a que se procesen las filas
    if err := writer.Flush(); err != nil {
        return fmt.Errorf("Error al procesar las filas: %v", err)
    }

    return nil
}

// Esta función exporta los datos de una tabla de BigQuery a un archivo CSV.
func exportCSVFromBigQuery(ctx context.Context, projectID string, datasetID string, tableID string, csvFile string) error {
    // Creamos un cliente de BigQuery
    client, err := bigquery.NewClient(ctx, projectID)
    if err != nil {
        return fmt.Errorf("Error al crear el cliente de BigQuery: %v", err)
    }
    defer client.Close()

    // Creamos un conjunto de resultados
    results, err := client.Query(fmt.Sprintf("SELECT * FROM `%s.%s.%s`", projectID, datasetID, tableID)).Read(ctx)
    if err != nil {
        return fmt.Errorf("Error al crear el conjunto de resultados: %v", err)
    }

    // Creamos un archivo CSV
    file, err := os.Create(csvFile)
    if err != nil {
        return fmt.Errorf("Error al crear el archivo CSV: %v", err)
    }
    defer file.Close()

    // Creamos un escritor CSV
    writer := csv.NewWriter(file)

    // Escribimos la cabecera en el archivo CSV
    header := make([]string, 0)
    for _, field := range results.Schema {
        header = append(header, field.Name)
    }
    if err := writer.Write(header); err != nil {
        return fmt.Errorf("Error al escribir la cabecera en el archivo CSV: %v", err)
    }

    // Escribimos las filas en el archivo CSV
    for {
        row, err := results.Next()
        if err == io.EOF {
            break
        }
        if err != nil {
            return fmt.Errorf("Error al obtener la siguiente fila: %v", err)
        }

        // Convertimos la fila en un array de valores
        values := make([]string, 0)
        for _, value := range row {
            values = append(values, value.String())
        }

        // Escribimos la fila en el archivo CSV
        if err := writer.Write(values); err != nil {
            return fmt.Errorf("Error al escribir la fila en el archivo CSV: %v", err)
        }
    }

    // Limpiamos el escritor CSV
    writer.Flush()

    return nil
}

// Esta función carga los datos de un archivo CSV a un depósito de GCS.
func uploadCSVToGCS(ctx context.Context, bucketName string, objectName string, csvFile string) error {
    // Creamos un cliente de GCS
    client, err := storage.NewClient(ctx)
    if err != nil {
        return fmt.Errorf("Error al crear el cliente de GCS: %v", err)
    }
    defer client.Close()

    // Creamos un depósito si no existe
    bucket := client.Bucket(bucketName)
    if err := bucket.Create(ctx, projectID, nil); err != nil {
        if !strings.Contains(err.Error(), "Bucket already exists") {
            return fmt.Errorf("Error al crear el depósito: %v", err)
        }
    }

    // Abrimos el archivo CSV
    file, err := os.Open(csvFile)
    if err != nil {
        return fmt.Errorf("Error al abrir el archivo CSV: %v", err)
    }
    defer file.Close()

    // Subimos el archivo CSV al depósito
    ctx, cancel := context.WithTimeout(ctx, time.Minute)
    defer cancel()
    if _, err := bucket.Object(objectName).CopierFrom(file).Run(ctx); err != nil {
        return fmt.Errorf("Error al subir el archivo CSV al depósito: %v", err)
    }

    return nil
}

func main() {
    // Configuramos las variables de entorno
    projectID := os.Getenv("GOOGLE_CLOUD_PROJECT")
    datasetID := os.Getenv("BIGQUERY_DATASET_ID")
    tableID := os.Getenv("BIGQUERY_TABLE_ID")
    csvFile