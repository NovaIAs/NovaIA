**Système de Gestion de Base de Données Distribuée avec Résilience (en Français)**

```go
package main

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"time"

	"cloud.google.com/go/spanner"
	"github.com/go-chi/chi"
	"github.com/go-chi/chi/middleware"
	"github.com/go-chi/render"
	"github.com/sirupsen/logrus"
)

// Application représente l'application principale.
type Application struct {
	client *spanner.Client
	router *chi.Mux
	logger *logrus.Logger
}

// Main est le point d'entrée de l'application.
func main() {
	logger := logrus.New()
	logger.SetLevel(logrus.InfoLevel)
	logger.SetOutput(os.Stdout)

	ctx := context.Background()
	client, err := spanner.NewClient(ctx, "projects/"+os.Getenv("GOOGLE_CLOUD_PROJECT")+"/instances/"+os.Getenv("SPANNER_INSTANCE")+"/databases/"+os.Getenv("SPANNER_DATABASE"))
	if err != nil {
		log.Fatalf("Impossible de créer le client Spanner : %v", err)
	}
	defer client.Close()

	app := &Application{
		client: client,
		router: chi.NewRouter(),
		logger: logger,
	}

	app.router.Use(middleware.RealIP)
	app.router.Use(middleware.Recoverer)
	app.router.Use(loggerMiddleware)

	app.router.Get("/albums", app.getAlbums)
	app.router.Post("/albums", app.createAlbum)
	app.router.Put("/albums/{id}", app.updateAlbum)
	app.router.Delete("/albums/{id}", app.deleteAlbum)

	http.ListenAndServe(":"+os.Getenv("PORT"), app.router)
}

// middlewareLogger est un middleware Chi qui enregistre les requêtes et les réponses.
func loggerMiddleware(next http.Handler) http.Handler {
	fn := func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		ww := middleware.NewWrapResponseWriter(w, r.ProtoMajor)

		next.ServeHTTP(ww, r)

		latency := time.Since(start)
		logger.WithFields(logrus.Fields{
			"method": r.Method,
			"path":   r.URL.Path,
			"status": ww.Status(),
			"size":   ww.BytesWritten(),
			"latency": latency,
		}).Info("Requête")
	}
	return http.HandlerFunc(fn)
}

// getAlbums gère les requêtes GET /albums et renvoie la liste des albums.
func (app *Application) getAlbums(w http.ResponseWriter, r *http.Request) {
	var albums []Album
	iter := app.client.Single().Query(r.Context(), spanner.Statement{
		SQL: `SELECT SingerId, AlbumId, AlbumTitle FROM Albums`,
	})
	defer iter.Stop()
	for {
		row, err := iter.Next()
		if err == iterator.Done {
			break
		}
		if err != nil {
			app.logger.Errorf("Impossible de lire la ligne : %v", err)
			http.Error(w, "Erreur interne", http.StatusInternalServerError)
			return
		}

		var a Album
		if err := row.Columns(&a.SingerId, &a.AlbumId, &a.AlbumTitle); err != nil {
			app.logger.Errorf("Impossible de récupérer les valeurs de la ligne : %v", err)
			http.Error(w, "Erreur interne", http.StatusInternalServerError)
			return
		}

		albums = append(albums, a)
	}

	render.JSON(w, r, albums)
}

// createAlbum gère les requêtes POST /albums et crée un nouvel album.
func (app *Application) createAlbum(w http.ResponseWriter, r *http.Request) {
	var a Album
	if err := json.NewDecoder(r.Body).Decode(&a); err != nil {
		app.logger.Errorf("Impossible de décoder le corps de la requête : %v", err)
		http.Error(w, "Corps de requête mal formé", http.StatusBadRequest)
		return
	}

	_, err := app.client.ReadWriteTransaction(r.Context(), func(ctx context.Context, txn *spanner.ReadWriteTransaction) error {
		stmt := spanner.Statement{
			SQL: `INSERT INTO Albums (SingerId, AlbumId, AlbumTitle) VALUES (@SingerId, @AlbumId, @AlbumTitle)`,
			Params: map[string]interface{}{
				"SingerId":    a.SingerId,
				"AlbumId":     a.AlbumId,
				"AlbumTitle":  a.AlbumTitle,
			},
		}
		rowCount, err := txn.Update(ctx, stmt)
		if err != nil {
			return err
		}

		if rowCount != 1 {
			return fmt.Errorf("impossible d'insérer l'album : nombre incorrect de lignes affectées (%d)", rowCount)
		}

		return nil
	})
	if err != nil {
		app.logger.Errorf("Impossible d'insérer l'album : %v", err)
		http.Error(w, "Erreur interne", http.StatusInternalServerError)
		return
	}

	render.JSON(w, r, a)
}

// updateAlbum gére les requêtes PUT /albums/{id} et met à jour un album existant.
func (app *Application) updateAlbum(w http.ResponseWriter, r *http.Request) {
	id := chi.URLParam(r, "id")

	var a Album
	if err := json.NewDecoder(r.Body).Decode(&a); err != nil {
		app.logger.Errorf("Impossible de décoder le corps de la requête : %v", err)
		http.Error(w, "Corps de requête mal formé", http.StatusBadRequest)
		return
	}

	_, err := app.client.ReadWriteTransaction(r.Context(), func(ctx context.Context, txn *spanner.ReadWriteTransaction) error {
		stmt := spanner.Statement{
			SQL: `UPDATE Albums SET AlbumTitle = @AlbumTitle WHERE SingerId = @SingerId AND AlbumId = @AlbumId`,
			Params: map[string]interface{}{
				"SingerId":    a.SingerId,
				"AlbumId":     a.AlbumId,
				"AlbumTitle":  a.AlbumTitle,
			},
		}
		rowCount, err := txn.Update(ctx, stmt)
		if err != nil {
			return err
		}

		if rowCount != 1 {
			return fmt.Errorf("impossible de mettre à jour l'album : nombre incorrect de lignes affectées (%d)", rowCount)
		}

		return nil
	})
	if err != nil {
		app.logger.Errorf("Impossible de mettre à jour l'album : %v", err)
		http.Error(w, "Erreur interne", http.StatusInternalServerError)
		return
	}

	render.JSON(w, r, a)
}

// deleteAlbum gére les requêtes DELETE /albums/{id} et supprime un album existant.
func (app *Application) deleteAlbum(w http.ResponseWriter, r *http.Request) {
	id := chi.URLParam(r, "id")

	_, err := app.client.ReadWriteTransaction(r.Context(), func(ctx context.Context, txn *spanner.ReadWriteTransaction) error {
		stmt := spanner.Statement{
			SQL: `DELETE FROM Albums WHERE SingerId = @SingerId AND AlbumId = @AlbumId`,
			Params: map[string]interface{}{
				"SingerId": id,
				"AlbumId":  id,
			},
		}
		rowCount, err := txn.Update(ctx, stmt)
		if err != nil {
			return err
		}

		if rowCount != 1 {
			return fmt.Errorf("impossible de supprimer l'album : nombre incorrect de lignes affectées (%d)", rowCount)
		}

		return nil
	})
	if err != nil {
		app.logger.Errorf("Impossible de supprimer l'album : %v", err)
		http.Error(w, "Erreur interne", http.StatusInternalServerError)
		return
	}

	io.WriteString(w, "OK")
}

// Album représente un enregistrement d'album dans la base de données.
type Album struct {
	SingerId  string `json:"