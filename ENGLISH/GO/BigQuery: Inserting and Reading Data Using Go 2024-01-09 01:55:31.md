```go
package main

import (
	"context"
	"fmt"
	"log"
	"math/rand"
	"time"

	"cloud.google.com/go/bigquery"
)

// generateData generates a slice of test data.
func generateData(numPoints int) []*bigquery.Value {
	rand := rand.New(rand.NewSource(time.Now().UnixNano()))
	data := make([]*bigquery.Value, numPoints)
	for i := 0; i < numPoints; i++ {
		data[i] = &bigquery.Value{
			"timestamp": time.Now(),
			"value":     rand.Float64(),
		}
	}
	return data
}

func main() {
	ctx := context.Background()
	client, err := bigquery.NewClient(ctx, "project-id")
	if err != nil {
		log.Fatalf("bigquery.NewClient: %v", err)
	}
	defer client.Close()

	// Create a new table in the "samples" dataset.
	table := client.Dataset("samples").Table("test_data")
	if err := table.Create(ctx, &bigquery.TableMetadata{}); err != nil {
		log.Fatalf("couldn't create table: %v", err)
	}

	// Insert data into the table.
	inserter := table.Inserter()
	data := generateData(100)
	if err := inserter.Put(ctx, data); err != nil {
		log.Fatalf("couldn't insert data: %v", err)
	}

	// Read data from the table.
	q := client.Query("SELECT timestamp, value FROM `samples.test_data`")
	it, err := q.Read(ctx)
	if err != nil {
		log.Fatalf("query.Read(): %v", err)
	}
	for {
		var row []bigquery.Value
		err := it.Next(&row)
		if err == bigquery.Done {
			break
		}
		if err != nil {
			log.Fatalf("it.Next(): %v", err)
		}
		fmt.Println(row)
	}
}
```

This code demonstrates creating a new table in BigQuery, inserting data into the table, and then reading data from the table. The code uses the BigQuery client library for Go to interact with BigQuery.

The `generateData` function generates a slice of test data. Each data point consists of a timestamp and a value. The timestamp is the current time, and the value is a random float64.

The `main` function creates a new table in the "samples" dataset. The table is named "test_data". The `main` function then inserts the test data into the table using the `Inserter` type.

The `main` function then reads data from the table using the `Query` type. The query selects the timestamp and value columns from the "test_data" table. The `main` function iterates over the results of the query and prints each row.