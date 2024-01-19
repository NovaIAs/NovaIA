```go
package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path"
	"sync"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/gorilla/mux"
)

// File represents a file in the S3 bucket
type File struct {
	Name      string    `json:"name"`
	Size      int64     `json:"size"`
	UploadDate time.Time `json:"upload_date"`
}

// FileUploader uploads files to an S3 bucket
type FileUploader struct {
	s3Client   *s3.S3
	bucketName string
}

// NewFileUploader creates a new FileUploader
func NewFileUploader(bucketName string) *FileUploader {
	sess, err := session.NewSession()
	if err != nil {
		log.Fatalf("failed to create session: %v", err)
	}

	return &FileUploader{
		s3Client:   s3.New(sess),
		bucketName: bucketName,
	}
}

// UploadFile uploads a file to the S3 bucket
func (u *FileUploader) UploadFile(file *os.File) (*File, error) {
	fileInfo, err := file.Stat()
	if err != nil {
		return nil, fmt.Errorf("failed to get file info: %v", err)
	}

	input := &s3.PutObjectInput{
		Bucket: aws.String(u.bucketName),
		Key:    aws.String(fileInfo.Name()),
		Body:   file,
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	result, err := u.s3Client.PutObjectWithContext(ctx, input)
	if err != nil {
		return nil, fmt.Errorf("failed to upload file: %v", err)
	}

	return &File{
		Name:      fileInfo.Name(),
		Size:      fileInfo.Size(),
		UploadDate: time.Now(),
	}, nil
}

// FileServer serves files from the S3 bucket
type FileServer struct {
	s3Client   *s3.S3
	bucketName string
}

// NewFileServer creates a new FileServer
func NewFileServer(bucketName string) *FileServer {
	sess, err := session.NewSession()
	if err != nil {
		log.Fatalf("failed to create session: %v", err)
	}

	return &FileServer{
		s3Client:   s3.New(sess),
		bucketName: bucketName,
	}
}

// ServeFile serves a file from the S3 bucket
func (s *FileServer) ServeFile(w http.ResponseWriter, r *http.Request) {
	fileName := path.Base(r.URL.Path)

	input := &s3.GetObjectInput{
		Bucket: aws.String(s.bucketName),
		Key:    aws.String(fileName),
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	result, err := s.s3Client.GetObjectWithContext(ctx, input)
	if err != nil {
		http.Error(w, "failed to get file", http.StatusInternalServerError)
		return
	}

	defer result.Body.Close()

	w.Header().Set("Content-Disposition", fmt.Sprintf("attachment; filename=%s", fileName))
	w.Header().Set("Content-Type", "application/octet-stream")
	http.ServeContent(w, r, fileName, time.Now(), result.ContentLength)
}

// GetFiles gets a list of files from the S3 bucket
func (s *FileServer) GetFiles(w http.ResponseWriter, r *http.Request) {
	input := &s3.ListObjectsInput{
		Bucket: aws.String(s.bucketName),
	}

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	result, err := s.s3Client.ListObjectsWithContext(ctx, input)
	if err != nil {
		http.Error(w, "failed to list files", http.StatusInternalServerError)
		return
	}

	files := make([]*File, 0, len(result.Contents))
	for _, object := range result.Contents {
		files = append(files, &File{
			Name:      *object.Key,
			Size:      *object.Size,
			UploadDate: *object.LastModified,
		})
	}

	json.NewEncoder(w).Encode(files)
}

func main() {
	bucketName := flag.String("bucket-name", "", "The name of the S3 bucket to use")
	flag.Parse()

	if *bucketName == "" {
		log.Fatal("missing bucket-name flag")
	}

	fileUploader := NewFileUploader(*bucketName)
	fileServer := NewFileServer(*bucketName)

	router := mux.NewRouter()
	router.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
		file, _, err := r.FormFile("file")
		if err != nil {
			http.Error(w, "failed to get file", http.StatusBadRequest)
			return
		}

		uploadedFile, err := fileUploader.UploadFile(file)
		if err != nil {
			http.Error(w, "failed to upload file", http.StatusInternalServerError)
			return
		}

		json.NewEncoder(w).Encode(uploadedFile)
	})
	router.HandleFunc("/files", fileServer.GetFiles)
	router.HandleFunc("/files/{fileName}", fileServer.ServeFile)

	srv := &http.Server{
		Addr: ":8080",
		Handler: router,
	}

	var wg sync.WaitGroup
	wg.Add(1)

	go func() {
		defer wg.Done()

		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatalf("failed to start server: %v", err)
		}
	}()

	log.Println("server started on port 8080")

	// Graceful shutdown
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt)
	<-c

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	srv.Shutdown(ctx)
	log.Println("server stopped")

	wg.Wait()
}
```

Explanation:

* The code creates a simple web server that allows users to upload and download files to an S3 bucket.
* The `FileUploader` struct is responsible for uploading files to the S3 bucket.
* The `FileServer` struct is responsible for serving files from the S3 bucket.
* The `main` function creates an instance of the `FileUploader` and `FileServer` structs, sets up the routing for the web server, and starts the server on port 8080.
* The server listens for incoming requests and responds to them accordingly.
* When a user uploads a file, the server uses the `FileUploader` struct to upload the file to the S3 bucket.
* When a user requests a file, the server uses the `FileServer` struct to serve the file from the S3 bucket.
* The server also provides an API endpoint to get a list of all the files in the S3 bucket.