```go
package main

import (
	"bytes"
	"crypto/md5"
	"encoding/base64"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
)

// Config represents the application configuration.
type Config struct {
	Port int
	Addr string
}

// App represents the application state.
type App struct {
	Config  *Config
	Message string
}

// Response is the struct used for API responses.
type Response struct {
	Status  string      `json:"status"`
	Message string      `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

func main() {
	// Parse command-line flags.
	port := flag.Int("port", 8080, "Port to listen on.")
	addr := flag.String("addr", "0.0.0.0", "IP address to listen on.")
	flag.Parse()

	// Create a new config.
	config := &Config{
		Port: *port,
		Addr: *addr,
	}

	// Create a new app.
	app := &App{
		Config:  config,
		Message: "Hello, world!",
	}

	// Create a new router.
	r := chi.NewRouter()

	// Add middleware to the router.
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)

	// Add routes to the router.
	r.Get("/healthz", app.Healthz)
	r.Get("/message", app.GetMessage)
	r.Post("/message", app.SetMessage)
	r.Post("/file", app.UploadFile)

	// Start the server.
	log.Printf("Listening on %s:%d", config.Addr, config.Port)
	if err := http.ListenAndServe(fmt.Sprintf("%s:%d", config.Addr, config.Port), r); err != nil {
		log.Fatalf("Error starting server: %v", err)
	}
}

// Healthz is a simple health check endpoint.
func (app *App) Healthz(w http.ResponseWriter, r *http.Request) {
	render.JSON(w, r, &Response{
		Status:  "OK",
		Message: "I'm alive!",
	})
}

// GetMessage returns the app's message.
func (app *App) GetMessage(w http.ResponseWriter, r *http.Request) {
	render.JSON(w, r, &Response{
		Status:  "OK",
		Message: app.Message,
	})
}

func (app *App) UploadFile(w http.ResponseWriter, r *http.Request){
	// Parse multipart form data
	err := r.ParseMultipartForm(32 << 20)
	if err != nil {
		http.Error(w, "Error parsing multipart form: "+err.Error(), http.StatusBadRequest)
		return
	}
	
	// Get the file from the form
	file, _, err := r.FormFile("file")
	if err != nil {
		http.Error(w, "Error getting file from form: "+err.Error(), http.StatusBadRequest)
		return
	}
	defer file.Close()
	
	// Calculate the MD5 hash of the file
	hash := md5.New()
	if _, err := io.Copy(hash, file); err != nil {
		http.Error(w, "Error calculating MD5 hash: "+err.Error(), http.StatusInternalServerError)
		return
	}
	md5Hash := base64.StdEncoding.EncodeToString(hash.Sum(nil))
	
	// Save the file to disk
	filename := filepath.Join(os.TempDir(), "uploaded-file-"+md5Hash)
	outFile, err := os.Create(filename)
	if err != nil {
		http.Error(w, "Error saving file to disk: "+err.Error(), http.StatusInternalServerError)
		return
	}
	defer outFile.Close()
	
	if _, err := io.Copy(outFile, file); err != nil {
		http.Error(w, "Error copying file to disk: "+err.Error(), http.StatusInternalServerError)
		return
	}
	
	// Send a response to the client
	render.JSON(w, r, &Response{
		Status:  "OK",
		Message: "File uploaded successfully!",
		Data: map[string]interface{}{
			"filename": filename,
			"md5Hash":  md5Hash,
		},
	})
}
// SetMessage sets the app's message.
func (app *App) SetMessage(w http.ResponseWriter, r *http.Request) {
	// Decode the request body.
	var req struct {
		Message string `json:"message"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		render.JSON(w, r, &Response{
			Status:  "Error",
			Message: "Error decoding request body: " + err.Error(),
		})
		return
	}

	// Set the app's message.
	app.Message = req.Message

	// Send a response to the client.
	render.JSON(w, r, &Response{
		Status:  "OK",
		Message: "Message set successfully!",
	})
}

func getProcessInfo(pid int) (*exec.Cmd, error) {
	cmd := exec.Command("ps", "-o", "comm,rss", "-p", strconv.Itoa(pid))
	return cmd, nil
}

func getMemoryUsage(pid int) (int, error){
	cmd, err := getProcessInfo(pid)
	if err != nil {
		return 0, err
	}
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	if err := cmd.Run(); err != nil {
		return 0, err
	}
	output := strings.Split(stdout.String(), "\n")
	if len(output) < 2 {
		return 0, fmt.Errorf("Unexpected output from ps -o comm,rss -p %d: %s", pid, stdout.String())
	}
	rss, err := strconv.Atoi(strings.TrimSpace(output[1]))
	if err != nil {
		return 0, fmt.Errorf("Error converting RSS memory usage to int: %v", err)
	}
	return rss, nil
}

func logProcessInfo(pid, interval int) {
	for {
		rss, err := getMemoryUsage(pid)
		if err != nil {
			log.Printf("Error getting memory usage for process with PID %d: %v", pid, err)
		} else {
			log.Printf("Process with PID %d is using %d bytes of memory.", pid, rss)
		}
		time.Sleep(time.Duration(interval) * time.Second)
	}
}

func runProcess(cmd *exec.Cmd) (int, error) {
	// Start the process.
	if err := cmd.Start(); err != nil {
		return 0, err
	}

	// Get the process ID.
	pid := cmd.Process.Pid

	// Log the process info.
	go logProcessInfo(pid, 1)

	// Wait for the process to finish.
	if err := cmd.Wait(); err != nil {
		return 0, err
	}

	return pid, nil
}

//run() executes a command string and returns its output.
func run(command string) (string, error) {
	// Create a new command.
	cmd := exec.Command("bash", "-c", command)

	// Run the command.
	out, err := cmd.CombinedOutput()

	// Return the output or the error.
	if err != nil {
		return "", err
	}
	return string(out), nil
}

func main() {
	// Parse command-line flags.
	command := flag.String("command", "", "The command to execute.")
	flag.Parse()

	// Run the command.
	output, err := run(*command)

	// Print the output or the error.
	if err != nil