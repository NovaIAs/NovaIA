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
	"strings"
	"time"

	"cloud.google.com/go/pubsub"
	"github.com/gorilla/mux"
	"github.com/sendgrid/sendgrid-go"
	"github.com/sendgrid/sendgrid-go/helpers/mail"
)

// Message represents a message published to a Pub/Sub topic.
type Message struct {
	Email   string
	Message string
}

// Subscribe pulls messages from a Pub/Sub subscription.
func Subscribe(ctx context.Context, sub *pubsub.Subscription) {
	// Create a channel to receive messages.
	ch := make(chan *pubsub.Message)

	// Receive messages from the subscription.
	err := sub.Receive(ctx, ch)
	if err != nil {
		log.Fatalf("Failed to receive messages: %v", err)
	}

	// Process messages.
	go func() {
		for msg := range ch {
			// Decode the message data.
			var message Message
			err := json.Unmarshal(msg.Data, &message)
			if err != nil {
				log.Printf("Failed to decode message: %v", err)
				msg.Nack()
				continue
			}

			// Send the email.
			err = SendEmail(message.Email, message.Message)
			if err != nil {
				log.Printf("Failed to send email: %v", err)
				msg.Nack()
				continue
			}

			// Acknowledge the message.
			msg.Ack()
		}
	}()
}

// SendEmail sends an email using the SendGrid API.
func SendEmail(to string, message string) error {
	// Create a SendGrid client.
	client := sendgrid.NewSendClient(os.Getenv("SENDGRID_API_KEY"))

	// Create an email.
	from := mail.NewEmail("Example User", "example@example.com")
	subject := "Hello from Go!"
	plainTextContent := "This is a plain text email."
	htmlContent := "<strong>This is an HTML email.</strong>"
	email := mail.NewSingleEmail(from, subject, to, plainTextContent, htmlContent)

	// Send the email.
	_, err := client.Send(email)
	if err != nil {
		return err
	}

	return nil
}

// StartServer starts a web server that listens for HTTP requests.
func StartServer() {
	// Create a new router.
	r := mux.NewRouter()

	// Handle POST requests to the /email endpoint.
	r.HandleFunc("/email", func(w http.ResponseWriter, r *http.Request) {
		// Read the request body.
		body, err := ioutil.ReadAll(r.Body)
		if err != nil {
			http.Error(w, "Failed to read request body", http.StatusBadRequest)
			return
		}

		// Decode the request body.
		var request struct {
			Email   string
			Message string
		}
		err = json.Unmarshal(body, &request)
		if err != nil {
			http.Error(w, "Failed to decode request body", http.StatusBadRequest)
			return
		}

		// Validate the request.
		if request.Email == "" || request.Message == "" {
			http.Error(w, "Missing required fields", http.StatusBadRequest)
			return
		}

		// Publish the message to the Pub/Sub topic.
		topic := client.Topic("emails")
		result := topic.Publish(ctx, &pubsub.Message{
			Data: []byte(fmt.Sprintf(`{"email": "%s", "message": "%s"}`, request.Email, request.Message)),
		})
		_, err = result.Get(ctx)
		if err != nil {
			http.Error(w, "Failed to publish message", http.StatusInternalServerError)
			return
		}

		// Respond to the request.
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("Message sent"))
	})

	// Handle GET requests to the /healthz endpoint.
	r.HandleFunc("/healthz", func(w http.ResponseWriter, r *http.Request) {
		// Respond to the request.
		w.WriteHeader(http.StatusOK)
		w.Write([]byte("OK"))
	})

	// Start the server.
	log.Fatal(http.ListenAndServe(":8080", r))
}

func main() {
	// Set the context.
	ctx := context.Background()

	// Create a Pub/Sub client.
	client, err := pubsub.NewClient(ctx, "project-id")
	if err != nil {
		log.Fatalf("Failed to create Pub/Sub client: %v", err)
	}

	// Get the subscription.
	sub := client.Subscription("emails-sub")

	// Start the server.
	go StartServer()

	// Subscribe to the subscription.
	Subscribe(ctx, sub)

	// Keep the main goroutine alive.
	for {
		time.Sleep(1 * time.Second)
	}
}
```

This code is a complete web application that uses Pub/Sub to send emails. It consists of the following components:

* A Pub/Sub topic named "emails".
* A Pub/Sub subscription named "emails-sub".
* A web server that listens for HTTP requests.
* A Go client library for Pub/Sub.
* A Go client library for SendGrid.

The web server has two endpoints:

* The `/email` endpoint handles POST requests that contain an email address and a message.
* The `/healthz` endpoint handles GET requests and returns "OK" if the server is healthy.

When a POST request is made to the `/email` endpoint, the web server does the following:

1. Reads the request body.
2. Decodes the request body into a struct.
3. Validates the request.
4. Publishes the message to the Pub/Sub topic.
5. Responds to the request with "Message sent".

When a message is published to the Pub/Sub topic, the subscription delivers the message to a subscriber. In this case, the subscriber is a Go program that does the following:

1. Decodes the message data.
2. Sends the email using the SendGrid API.
3. Acknowledge the message.

The web server also has a `/healthz` endpoint that returns "OK" if the server is healthy. This endpoint can be used to check the health of the server.

The main function of the program starts the web server and subscribes to the Pub/Sub subscription. The program then keeps the main goroutine alive so that the web server and subscriber can continue to run.