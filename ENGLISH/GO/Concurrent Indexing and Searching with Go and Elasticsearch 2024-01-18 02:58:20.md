```go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"strconv"
	"sync"
	"time"

	"github.com/elastic/go-elasticsearch/v7"
	"github.com/elastic/go-elasticsearch/v7/esapi"
	uuid "github.com/satori/go.uuid"
)

// Number of concurrent HTTP requests to be made
var numConcurrentRequests = 10

// Number of documents to be indexed
var numDocuments = 1000

// Elasticsearch client
var esClient *elasticsearch.Client

// Index name
var indexName = "test-index"

// Type name
var typeName = "test-type"

// Create an Elasticsearch client
func createClient() (*elasticsearch.Client, error) {
	// Get the Elasticsearch host and port from environment variables
	elasticHost := os.Getenv("ELASTICSEARCH_HOST")
	elasticPort := os.Getenv("ELASTICSEARCH_PORT")

	// Create the Elasticsearch client
	cfg := elasticsearch.Config{
		Addresses: []string{fmt.Sprintf("http://%s:%s", elasticHost, elasticPort)},
	}
	client, err := elasticsearch.NewClient(cfg)
	if err != nil {
		return nil, err
	}

	// Ping the Elasticsearch cluster to check if it is up
	ctx := context.Background()
	if _, err := client.Ping(ctx); err != nil {
		return nil, err
	}

	return client, nil
}

// Create an index in Elasticsearch
func createIndex(client *elasticsearch.Client) error {
	// Create the index request
	req := esapi.IndicesCreateRequest{
		Index: indexName,
	}

	// Send the request and check the response
	res, err := client.Indices.Create(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()

	if res.IsError() {
		return fmt.Errorf("Error creating index: %s", res.String())
	}

	return nil
}

// Index a document in Elasticsearch
func indexDocument(client *elasticsearch.Client, id string, document map[string]interface{}) error {
	// Create the index request
	req := esapi.IndexRequest{
		Index:      indexName,
		DocumentID: id,
		Body:       document,
	}

	// Send the request and check the response
	res, err := client.Index(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()

	if res.IsError() {
		return fmt.Errorf("Error indexing document: %s", res.String())
	}

	return nil
}

// Generate a random document
func generateDocument() map[string]interface{} {
	// Create a map to store the document
	document := make(map[string]interface{})

	// Generate a random ID for the document
	document["id"] = uuid.NewV4().String()

	// Generate a random name for the document
	document["name"] = fmt.Sprintf("Document %d", document["id"])

	// Generate a random description for the document
	document["description"] = fmt.Sprintf("This is a random document with ID %s", document["id"])

	// Generate a random timestamp for the document
	document["timestamp"] = time.Now().UnixNano()

	// Return the document
	return document
}

// Create a channel for documents to be indexed
var documentsChannel = make(chan map[string]interface{}, numDocuments)

// Index documents in Elasticsearch concurrently
func indexDocuments(client *elasticsearch.Client) {
	// Create a wait group to wait for all goroutines to finish
	var wg sync.WaitGroup

	// Start goroutines to index documents concurrently
	for i := 0; i < numConcurrentRequests; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()

			for document := range documentsChannel {
				err := indexDocument(client, document["id"].(string), document)
				if err != nil {
					log.Printf("Error indexing document: %s", err)
				}
			}
		}()
	}

	// Close the documents channel to signal that all documents have been sent
	close(documentsChannel)

	// Wait for all goroutines to finish
	wg.Wait()
}

// Search for documents in Elasticsearch
func searchDocuments(client *elasticsearch.Client, query string) ([]map[string]interface{}, error) {
	// Create the search request
	req := esapi.SearchRequest{
		Index: []string{indexName},
		Query: map[string]interface{}{
			"query": map[string]interface{}{
				"match": map[string]interface{}{
					"name": query,
				},
			},
		},
	}

	// Send the request and check the response
	res, err := client.Search(req)
	if err != nil {
		return nil, err
	}
	defer res.Body.Close()

	if res.IsError() {
		return nil, fmt.Errorf("Error searching for documents: %s", res.String())
	}

	// Parse the response body
	var hits struct {
		Hits struct {
			Hits []struct {
				Source map[string]interface{}
			}
		}
	}
	if err := json.NewDecoder(res.Body).Decode(&hits); err != nil {
		return nil, err
	}

	// Extract the documents from the response
	documents := make([]map[string]interface{}, len(hits.Hits.Hits))
	for i, hit := range hits.Hits.Hits {
		documents[i] = hit.Source
	}

	// Return the documents
	return documents, nil
}

// Delete an index in Elasticsearch
func deleteIndex(client *elasticsearch.Client) error {
	// Create the delete index request
	req := esapi.IndicesDeleteRequest{
		Index: indexName,
	}

	// Send the request and check the response
	res, err := client.Indices.Delete(req)
	if err != nil {
		return err
	}
	defer res.Body.Close()

	if res.IsError() {
		return fmt.Errorf("Error deleting index: %s", res.String())
	}

	return nil
}

// Main function
func main() {
	// Create an Elasticsearch client
	client, err := createClient()
	if err != nil {
		log.Fatalf("Error creating Elasticsearch client: %s", err)
	}

	// Create the index
	err = createIndex(client)
	if err != nil {
		log.Fatalf("Error creating index: %s", err)
	}

	// Generate documents and send them to the channel concurrently
	go func() {
		for i := 0; i < numDocuments; i++ {
			documentsChannel <- generateDocument()
		}
	}()

	// Index the documents concurrently
	indexDocuments(client)

	// Search for documents
	query := "Document 1"
	documents, err := searchDocuments(client, query)
	if err != nil {
		log.Fatalf("Error searching for documents: %s", err)
	}

	// Print the found documents
	fmt.Println("Found documents:")
	for _, document := range documents {
		fmt.Println(document)
	}

	// Delete the index
	err = deleteIndex(client)
	if err != nil {
		log.Fatalf("Error deleting index: %s", err)
	}
}
```