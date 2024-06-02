**Système de gestion de bibliothèque dynamique en Python**

**Importation des bibliothèques**

```python
import sqlite3
import tkinter as tk
from tkinter import ttk, messagebox
```

**Création de la base de données**

```python
conn = sqlite3.connect('bibliotheque.db')
c = conn.cursor()
c.execute('''CREATE TABLE IF NOT EXISTS Livres (
    ISBN TEXT PRIMARY KEY,
    titre TEXT NOT NULL,
    auteur TEXT NOT NULL,
    genre TEXT NOT NULL,
    stock INT NOT NULL
)''')
conn.commit()
```

**Fenêtre principale**

```python
fenetre = tk.Tk()
fenetre.title("Gestion de bibliothèque")
fenetre.geometry("800x600")
```

**Onglets**

```python
onglets = ttk.Notebook(fenetre)
onglets.pack(expand=True, fill='both')
```

**Onglet "Livres"**

```python
cadre_livres = tk.Frame(onglets)
onglets.add(cadre_livres, text="Livres")

label_isbn = tk.Label(cadre_livres, text="ISBN:")
entry_isbn = tk.Entry(cadre_livres)

label_titre = tk.Label(cadre_livres, text="Titre:")
entry_titre = tk.Entry(cadre_livres)

label_auteur = tk.Label(cadre_livres, text="Auteur:")
entry_auteur = tk.Entry(cadre_livres)

label_genre = tk.Label(cadre_livres, text="Genre:")
entry_genre = tk.Entry(cadre_livres)

label_stock = tk.Label(cadre_livres, text="Stock:")
entry_stock = tk.Entry(cadre_livres)

btn_ajouter = tk.Button(cadre_livres, text="Ajouter")
btn_modifier = tk.Button(cadre_livres, text="Modifier")
btn_supprimer = tk.Button(cadre_livres, text="Supprimer")

treeview_livres = ttk.Treeview(cadre_livres, columns=("ISBN", "Titre", "Auteur", "Genre", "Stock"))
treeview_livres.heading("#0", text="")
treeview_livres.heading("#1", text="ISBN")
treeview_livres.heading("#2", text="Titre")
treeview_livres.heading("#3", text="Auteur")
treeview_livres.heading("#4", text="Genre")
treeview_livres.heading("#5", text="Stock")
treeview_livres.pack(fill='both', expand=True)
```

**Onglet "Emprunts"**

```python
cadre_emprunts = tk.Frame(onglets)
onglets.add(cadre_emprunts, text="Emprunts")

label_isbn_emprunt = tk.Label(cadre_emprunts, text="ISBN du livre:")
entry_isbn_emprunt = tk.Entry(cadre_emprunts)

label_nom_emprunteur = tk.Label(cadre_emprunts, text="Nom de l'emprunteur:")
entry_nom_emprunteur = tk.Entry(cadre_emprunts)

btn_emprunter = tk.Button(cadre_emprunts, text="Emprunter")
btn_rendre = tk.Button(cadre_emprunts, text="Rendre")

treeview_emprunts = ttk.Treeview(cadre_emprunts, columns=("ISBN", "titre", "auteur", "emprunteur", "date d'emprunt"))
treeview_emprunts.heading("#0", text="")
treeview_emprunts.heading("#1", text="ISBN")
treeview_emprunts.heading("#2", text="Titre")
treeview_emprunts.heading("#3", text="Auteur")
treeview_emprunts.heading("#4", text="Emprunteur")
treeview_emprunts.heading("#5", text="Date d'emprunt")
treeview_emprunts.pack(fill='both', expand=True)
```

**Fonctions**

```python
def ajouter_livre():
    # Récupération des données du formulaire
    isbn = entry_isbn.get()
    titre = entry_titre.get()
    auteur = entry_auteur.get()
    genre = entry_genre.get()
    stock = int(entry_stock.get())

    # Insertion des données dans la base de données
    try:
        c.execute("INSERT INTO Livres VALUES (?, ?, ?, ?, ?)", (isbn, titre, auteur, genre, stock))
        conn.commit()
        messagebox.showinfo("Succès", "Le livre a été ajouté avec succès.")
    except sqlite3.IntegrityError:
        messagebox.showerror("Erreur", "L'ISBN existe déjà.")
    finally:
        # Actualisation de l'affichage
        treeview_livres.delete(*treeview_livres.get_children())
        afficher_livres()

def modifier_livre():
    # Récupération des données du formulaire
    isbn = entry_isbn.get()
    titre = entry_titre.get()
    auteur = entry_auteur.get()
    genre = entry_genre.get()
    stock = int(entry_stock.get())

    # Mise à jour des données dans la base de données
    try:
        c.execute("UPDATE Livres SET titre = ?, auteur = ?, genre = ?, stock = ? WHERE ISBN = ?", (titre, auteur, genre, stock, isbn))
        conn.commit()
        messagebox.showinfo("Succès", "Le livre a été modifié avec succès.")
    except sqlite3.IntegrityError:
        messagebox.showerror("Erreur", "L'ISBN n'existe pas.")
    finally:
        # Actualisation de l'affichage
        treeview_livres.delete(*treeview_livres.get_children())
        afficher_livres()

def supprimer_livre():
    # Récupération de l'ISBN sélectionné
    isbn = entry_isbn.get()

    # Suppression des données dans la base de données
    try:
        c.execute("DELETE FROM Livres WHERE ISBN = ?", (isbn,))
        conn.commit()
        messagebox.showinfo("Succès", "Le livre a été supprimé avec succès.")
    except sqlite3.IntegrityError:
        messagebox.showerror("Erreur", "L'ISBN n'existe pas.")
    finally:
        # Actualisation de l'affichage
        treeview_livres.delete(*treeview_livres.get_children())
        afficher_livres()

def afficher_livres():
    # Récupération des données de la base de données
    c.execute("SELECT * FROM Livres")
    livres = c.fetchall()

    # Affichage des données dans le treeview
    for livre in livres:
        treeview_livres.insert('', 'end', values=livre)

def ajouter_emprunt():
    # Récupération des données du formulaire
    isbn = entry_isbn_emprunt.get()
    nom_emprunteur = entry_nom_emprunteur.get()

    # Vérification de l'existence du livre
    c.execute("SELECT stock FROM Livres WHERE ISBN = ?", (isbn,))
    stock = c.fetchone()[0]

    if stock > 0:
        # Insertion des données dans la base de données
        c.execute("INSERT INTO Emprunts (ISBN, emprunteur, date_emprunt) VALUES (?, ?, date('now'))", (isbn, nom_emprunteur))
        conn.commit()

        # Déduction du stock dans la base de données
        c.execute("UPDATE Livres SET stock = stock - 1 WHERE ISBN = ?", (isbn,))
        conn.commit()

        messagebox.showinfo("Succès", "Le livre a été emprunté avec succès.")
    else:
        messagebox.showerror("Erreur", "Le livre n'est pas disponible.")

    # Actualisation de l'affichage
    treeview_emprunts.delete(*treeview_emprunts.get_children())
    afficher_emprunts()

def rendre_emprunt():
    # Récupération de l'ISBN sélectionné
    isbn = entry_isbn_emprunt.get()

    # Vérification de l'existence de l'emprunt
    c.execute("SELECT * FROM Emprunts WHERE ISBN = ?", (isbn,))
    emprunt = c.fetchone()

    if emprunt:
        # Suppression des données dans la base de données
        c.execute("DELETE FROM Emprunts WHERE ISBN = ?", (isbn,))
        conn.commit()

        # Incrémentation du stock dans la base de données
        c.execute("UPDATE Livres SET stock = stock + 1 WHERE ISBN = ?", (isbn,))
        conn.commit()

        messagebox.showinfo("Succès", "Le livre a été rendu avec succès.")
    else:
        messagebox.showerror("Erreur", "L'emprunt