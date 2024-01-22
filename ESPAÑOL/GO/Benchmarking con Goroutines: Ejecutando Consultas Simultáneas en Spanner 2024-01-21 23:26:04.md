```go
package main

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"os"
	"strconv"
	"sync"
	"time"

	"cloud.google.com/go/spanner"
	"google.golang.org/api/iterator"
)

const (
	dbName  = "projects/proj_name/instances/instance_name/databases/db_name"
	rowCount = 10_000_000
)

// genera una entrada de benchamrk con un campo ENTERO
func genBenchmarkEntry(id int64) *spanner.Mutation {
	return spanner.InsertOrUpdate("Benchmark", []string{"id", "value"}, []interface{}{id, rand.Float64()})
}

// genera n entradas de benchmark aleatorias
func genBenchmarkEntries() []*spanner.Mutation {
	entries := make([]*spanner.Mutation, rowCount)
	for id := 0; id < rowCount; id++ {
		entries[id] = genBenchmarkEntry(int64(id))
	}
	return entries
}


func main() {
	ctx := context.Background()
	client, err := spanner.NewClient(ctx, dbName)
	if err != nil {
		log.Fatalf("spanner.NewClient: %v", err)
	}
	defer client.Close()

	// genera un número aleatorio de sesiones
	sessCount := rand.Intn(5) + 1
	sessions := make([]*spanner.Session, sessCount)

	err = client.Apply(ctx, genBenchmarkEntries())
	if err != nil {
		log.Fatalf("client.Apply: %v", err)
	}

	// Inicia múltiples goroutines para ejecutar consultas simultáneamente.
	var wg sync.WaitGroup
	for i := 0; i < sessCount; i++ {
		sessions[i] = client.Single()
		wg.Add(1)
		go func(s *spanner.Session) {
			defer wg.Done()
			runQuery(s)
		}(sessions[i])
	}
	wg.Wait()
}

// ejecuta una consulta aleatoria en la tabla Benchmark
func runQuery(s *spanner.Session) {
	stmt := spanner.Statement{SQL: "SELECT id, value FROM Benchmark ORDER BY ABS(RANDOM()) LIMIT 100"}
	iter := s.Execute(ctx, stmt)
	defer iter.Stop()
	for {
		row, err := iter.Next()
		if err == iterator.Done {
			return
		}
		if err != nil {
			log.Fatal(err)
		}
		var id int64
		var value float64
		if err := row.Columns(&id, &value); err != nil {
			log.Fatal(err)
		}
		fmt.Fprintf(os.Stdout, "%10d %10.2f\n", id, value)
	}
}
```

Este código es un ejemplo de un benchmark que utiliza Spanner para generar y ejecutar consultas simultáneas en una base de datos. El código hace lo siguiente:

1. Genera un número aleatorio de sessions, que es el número de conexiones concurrentes que se utilizan para ejecutar las consultas.

1. Genera un número aleatorio de entradas en la tabla Benchmark. Cada entrada es una fila con dos columnas: "id" y "value".

1. Inicia múltiples goroutines, una para cada session, para ejecutar consultas simultáneamente en la tabla Benchmark. Cada goroutine ejecuta una consulta diferente, ordenada aleatoriamente.

1. La función `runQuery()` ejecuta una consulta aleatoria en la tabla Benchmark y devuelve los resultados.

Este código es complejo porque utiliza múltiples goroutines para ejecutar consultas simultáneamente. Esto puede ser difícil de depurar y puede conducir a errores de concurrencia. Sin embargo, este código también es un buen ejemplo de cómo se puede utilizar Spanner para ejecutar consultas simultáneas en una base de datos.