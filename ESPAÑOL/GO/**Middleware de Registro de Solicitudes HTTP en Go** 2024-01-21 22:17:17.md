```go
package main

import (
	"bytes"
	"context"
	"fmt"
	"html/template"
	"io"
	"log"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"

	"cloud.google.com/go/datastore"
	"github.com/google/uuid"
)

// DatastoreClient es el cliente de Datastore.
var DatastoreClient *datastore.Client

// Cache es un mapa global que almacena los datos recuperados.
var Cache = sync.Map{}

// LogRequest es una estructura para registrar las solicitudes HTTP.
type LogRequest struct {
	Method  string
	URL     string
	Proto   string
	Headers map[string][]string
	Body    []byte
}

// LogEntry es una estructura para registrar las entradas de log.
type LogEntry struct {
	ID         string
	TimeStamp  time.Time
	Severity   string
	Message    string
	RequestLog LogRequest
}

// LogEntryKey es la clave de la entidad LogEntry en Datastore.
type LogEntryKey struct {
	ID string
}

// CreateLogEntry crea una nueva entidad LogEntry en Datastore.
func CreateLogEntry(ctx context.Context, logEntry *LogEntry) (string, error) {
	key := DatastoreClient.IncompleteKey("LogEntry", nil)
	key, err := DatastoreClient.Put(ctx, key, logEntry)
	if err != nil {
		return "", err
	}
	return key.ID, nil
}

// GetLogEntry recupera una entidad LogEntry de Datastore.
func GetLogEntry(ctx context.Context, id string) (*LogEntry, error) {
	key := datastore.NameKey("LogEntry", id, nil)
	logEntry := &LogEntry{}
	if err := DatastoreClient.Get(ctx, key, logEntry); err != nil {
		return nil, err
	}
	return logEntry, nil
}

// QueryLogEntries recupera una lista de entidades LogEntry de Datastore.
func QueryLogEntries(ctx context.Context, query *datastore.Query) ([]*LogEntry, error) {
	var logEntries []*LogEntry
	if _, err := DatastoreClient.GetAll(ctx, query, &logEntries); err != nil {
		return nil, err
	}
	return logEntries, nil
}

// CacheLogEntry almacena una entidad LogEntry en el mapa global Cache.
func CacheLogEntry(logEntry *LogEntry) {
	Cache.Store(logEntry.ID, logEntry)
}

// GetCachedLogEntry recupera una entidad LogEntry del mapa global Cache.
func GetCachedLogEntry(id string) (*LogEntry, bool) {
	v, ok := Cache.Load(id)
	if !ok {
		return nil, false
	}
	logEntry, ok := v.(*LogEntry)
	return logEntry, ok
}

// LogHandler es un middleware que registra las solicitudes HTTP.
func LogHandler(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		logEntry := &LogEntry{
			ID:         uuid.New().String(),
			TimeStamp:  time.Now(),
			Severity:   "INFO",
			Message:    fmt.Sprintf("%s %s", r.Method, r.URL.Path),
			RequestLog: LogRequest{
				Method:  r.Method,
				URL:     r.URL.String(),
				Proto:   r.Proto,
				Headers: r.Header,
				Body:    ReadBody(r.Body),
			},
		}

		CacheLogEntry(logEntry)

		ctx := context.WithValue(r.Context(), "LogEntry", logEntry)
		next.ServeHTTP(w, r.WithContext(ctx))
	})
}

// ReadBody lee el cuerpo de la solicitud HTTP.
func ReadBody(body io.ReadCloser) []byte {
	buf := new(bytes.Buffer)
	if _, err := buf.ReadFrom(body); err != nil {
		return nil
	}
	// body.Close()
	return buf.Bytes()
}

// ErrorHandler es un middleware que maneja los errores de HTTP.
func ErrorHandler(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		defer func() {
			if err := recover(); err != nil {
				http.Error(w, http.StatusText(http.StatusInternalServerError), http.StatusInternalServerError)
			}
		}()
		next.ServeHTTP(w, r)
	})
}

// MustGetLogEntry es una función que recupera una entidad LogEntry de Datastore o devuelve un error si no se encuentra.
func MustGetLogEntry(ctx context.Context, id string) *LogEntry {
	logEntry, err := GetLogEntry(ctx, id)
	if err != nil {
		log.Fatalf("Error al recuperar la entrada de registro: %v", err)
	}
	return logEntry
}

// IndexHandler maneja las solicitudes HTTP para la página de índice.
func IndexHandler(w http.ResponseWriter, r *http.Request) {
	ctx := r.Context()
	logEntry := ctx.Value("LogEntry").(*LogEntry)

	logEntries, err := QueryLogEntries(ctx, datastore.NewQuery("LogEntry").Order("-TimeStamp"))
	if err != nil {
		log.Fatalf("Error al recuperar las entradas de registro: %v", err)
	}

	template, err := template.New("index").Parse(`
<!DOCTYPE html>
<html>
<head>
  <title>Index</title>
</head>
<body>
  <h1>Index</h1>
  <p>Log Entry ID: {{ .ID }}</p>
  <p>Log Entry Timestamp: {{ .TimeStamp }}</p>
  <p>Log Entry Severity: {{ .Severity }}</p>
  <p>Log Entry Message: {{ .Message }}</p>
  <p>Log Entry Request Method: {{ .RequestLog.Method }}</p>
  <p>Log Entry Request URL: {{ .RequestLog.URL }}</p>
  <p>Log Entry Request Proto: {{ .RequestLog.Proto }}</p>
  <p>Log Entry Request Headers:</p>
  <ul>
    {{ range $key, $value := .RequestLog.Headers }}
      <li>{{ $key }}: {{ $value }}</li>
    {{ end }}
  </ul>
  <p>Log Entry Request Body:</p>
  <pre>{{ .RequestLog.Body }}</pre>
  <hr>
  <h1>Log Entries</h1>
  <ul>
    {{ range .LogEntries }}
      <li><a href="/log/{{ .ID }}">{{ .ID }}</a></li>
    {{ end }}
  </ul>
</body>
</html>
	`)
	if err != nil {
		log.Fatalf("Error al parsear la plantilla: %v", err)
	}

	if err := template.Execute(w, map[string]interface{}{
		"LogEntry":    logEntry,
		"LogEntries": logEntries,
	}); err != nil {
		log.Fatalf("Error al ejecutar la plantilla: %v", err)
	}
}

// LogHandler maneja las solicitudes HTTP para la página de registro.
func LogHandler(w http.ResponseWriter, r *http.Request) {
	id := strings.TrimPrefix(r.URL.Path, "/log/")
	ctx := r.Context()

	logEntry, ok := GetCachedLogEntry(id)
	if !ok {
		logEntry = MustGetLogEntry(ctx, id)
	}

	template, err := template.New("log").Parse(`
<!DOCTYPE html>
<html>
<head>
  <title>Log</title>
</head>
<body>
  <h1>Log Entry</h1>
  <p>Log Entry ID: {{ .ID }}</p>
  <p>Log Entry Timestamp: {{ .TimeStamp }}</p>
  <p>Log Entry Severity: {{ .Severity }}</p>
  <p>Log Entry Message: {{ .Message }}</p>
  <p>Log Entry Request Method: {{ .RequestLog.Method }}</p>
  <p>Log Entry Request URL: {{ .RequestLog.URL }}</p>
  <p>Log Entry Request Proto: {{ .RequestLog.Proto }}</p>
  <p>Log Entry Request Headers:</p>
  <ul>
    {{ range $key, $value := .RequestLog.Headers }}
      <li>{{ $key }}: {{ $value }}</li>
    {{ end }}
  </ul>
  <p>Log Entry Request Body:</p>
  <pre>{{ .RequestLog.Body }}</pre>
</body>
</html>
	`)
	if err != nil {
		log.Fatalf("Error al parsear la plantilla: %v", err)
	}

	if err := template.Execute(w, logEntry); err != nil {
		log.Fatalf("Error al ejecutar la plantilla: %v", err)