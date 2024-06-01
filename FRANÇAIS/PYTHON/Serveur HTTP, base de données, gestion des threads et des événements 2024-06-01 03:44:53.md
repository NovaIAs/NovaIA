```python
# Importation des modules nécessaires
import os
import re
import json
import threading
import socketserver as ss
import http.server as hs

# Classe serveur HTTP
class MonServeurHTTP(hs.HTTPServer):

    # Répertoire racine du serveur
    document_root = os.path.dirname(os.path.abspath(__file__))

    # Gestion des requêtes HTTP
    def do_GET(self):
        # Récupération du chemin demandé
        chemin = self.path

        # Si le chemin est vide, on renvoie la page d'accueil
        if chemin == '/':
            chemin = '/index.html'

        # Récupération du fichier demandé
        try:
            fichier = open(self.document_root + chemin, 'rb')
            contenu = fichier.read()
            fichier.close()
        except FileNotFoundError:
            # Si le fichier n'existe pas, on renvoie une erreur 404
            self.erreur(404)
            return

        # Envoi de l'entête HTTP
        self.send_response(200)
        self.send_header('Content-Type', 'text/html')
        self.send_header('Content-Length', len(contenu))
        self.end_headers()

        # Envoi du contenu du fichier
        self.wfile.write(contenu)

    # Gestion des erreurs HTTP
    def erreur(self, code):
        self.send_response(code)
        self.send_header('Content-Type', 'text/html')
        self.send_header('Content-Length', len(str(code)))
        self.end_headers()
        self.wfile.write(str(code).encode('utf-8'))

# Classe client HTTP
class MonClientHTTP:

    # Adresse IP du serveur
    ip_serveur = '127.0.0.1'

    # Port du serveur
    port_serveur = 8080

    # Envoi d'une requête HTTP
    def envoyer_requete(self, chemin):
        # Création de la socket
        socket_client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        # Connexion au serveur
        socket_client.connect((self.ip_serveur, self.port_serveur))

        # Envoi de la requête
        requete = f"GET {chemin} HTTP/1.1\r\nHost: {self.ip_serveur}\r\n\r\n"
        socket_client.send(requete.encode('utf-8'))

        # Réception de la réponse
        reponse = socket_client.recv(4096)

        # Fermeture de la socket
        socket_client.close()

        # Renvoi de la réponse brute
        return reponse.decode('utf-8')

# Classe pour le traitement de la base de données
class BaseDeDonnees:

    # Fichier de la base de données
    fichier_bdd = 'bdd.json'

    # Chargement de la base de données
    def charger_bdd(self):
        try:
            with open(self.fichier_bdd, 'r') as fichier:
                self.bdd = json.load(fichier)
        except FileNotFoundError:
            self.bdd = {}

    # Sauvegarde de la base de données
    def sauvegarder_bdd(self):
        with open(self.fichier_bdd, 'w') as fichier:
            json.dump(self.bdd, fichier)

    # Ajout d'un élément à la base de données
    def ajouter(self, cle, valeur):
        self.bdd[cle] = valeur
        self.sauvegarder_bdd()

    # Récupération d'un élément de la base de données
    def recuperer(self, cle):
        return self.bdd.get(cle)

    # Suppression d'un élément de la base de données
    def supprimer(self, cle):
        if cle in self.bdd:
            del self.bdd[cle]
            self.sauvegarder_bdd()

# Classe pour la gestion des threads
class GestionThreads:

    # Liste des threads
    threads = []

    # Ajout d'un thread
    def ajouter_thread(self, thread):
        self.threads.append(thread)

    # Attente de la fin de tous les threads
    def attendre_fin(self):
        for thread in self.threads:
            thread.join()

# Classe pour la gestion des événements
class GestionEvenements:

    # Liste des événements
    evenements = []

    # Ajout d'un événement
    def ajouter_evenement(self, evenement):
        self.evenements.append(evenement)

    # Déclenchement d'un événement
    def declencher(self, evenement):
        for fonction in self.evenements[evenement]:
            fonction()

# Fonction principale
def main():
    try:
        # Création du serveur HTTP
        serveur = MonServeurHTTP(('', 8080), hs.SimpleHTTPRequestHandler)
        serveur_thread = threading.Thread(target=serveur.serve_forever)

        # Démarrage du serveur HTTP
        serveur_thread.start()

        # Chargement de la base de données
        bdd = BaseDeDonnees()
        bdd.charger_bdd()

        # Création du gestionnaire de threads
        gestion_threads = GestionThreads()

        # Création du gestionnaire d'événements
        gestion_evenements = GestionEvenements()

        # Ajout du thread du serveur à la liste des threads
        gestion_threads.ajouter_thread(serveur_thread)

        # Boucle de traitement principal
        while True:
            # Récupération des événements
            evenements = gestion_evenements.evenements

            # Traitement des événements
            for evenement in evenements:
                gestion_evenements.declencher(evenement)

            # Attente d'un événement
            evenement = input("> ")

            # Vérification de l'événement
            if evenement in evenements:
                gestion_evenements.declencher(evenement)
            elif evenement == 'quitter':
                break
            else:
                print("Erreur : événement inconnu")

        # Arrêt du serveur HTTP
        serveur.shutdown()

        # Attente de la fin de tous les threads
        gestion_threads.attendre_fin()

        # Sauvegarde de la base de données
        bdd.sauvegarder_bdd()

    except KeyboardInterrupt:
        # Arrêt du serveur HTTP
        serveur.shutdown()

        # Attente de la fin de tous les threads
        gestion_threads.attendre_fin()

        # Sauvegarde de la base de données
        bdd.sauvegarder_bdd()

# Appel de la fonction principale
if __name__ == '__main__':
    main()
```

**Explications du code**

Ce code Python implémente un système complexe qui combine un serveur HTTP, une base de données JSON, une gestion multithread et une gestion d'événements. Voici les principales fonctionnalités et leur fonctionnement :

**Serveur HTTP**

* La classe `MonServeurHTTP` hérite de la classe `hs.HTTPServer` pour créer un serveur HTTP qui sert des fichiers à partir d'un répertoire racine spécifié.
* La méthode `do_GET` gère les requêtes HTTP GET et renvoie le contenu du fichier demandé, ou une erreur 404 si le fichier n'est pas trouvé.

**Client HTTP**

* La classe `MonClientHTTP` fournit des fonctionnalités pour envoyer des requêtes HTTP à un serveur spécifié et recevoir des réponses.

**Base de données JSON**

* La classe `BaseDeDonnees` gère une base de données JSON stockée dans un fichier.
* Elle fournit des méthodes pour charger, sauvegarder, ajouter, récupérer et supprimer des éléments de la base de données.

**Gestion des threads**

* La classe `GestionThreads` permet d'ajouter et d'attendre la fin de plusieurs threads simultanés.

**Gestion des événements**

* La classe `GestionEvenements` gère une liste d'événements et de fonctions associées.
* Les événements peuvent être déclenchés pour exécuter les fonctions associées.

**Fonction principale**

* La fonction `main` est le point d'entrée du programme.
* Elle crée un serveur HTTP, charge la base de données, gère les threads et les événements, et s'exécute jusqu'à ce qu'un événement de fin soit déclenché ou que l'utilisateur entre la commande "quitter".

**Utilisation du code**

Pour utiliser ce code, vous pouvez :

1. Créer un fichier JSON "bdd.json" pour stocker la base de données.
2. Exécuter le code en exécutant la commande `python3 code_complexe.py`.
3. Naviguer vers `http://127.0.0.1:8080/` dans un navigateur pour accéder au serveur HTTP.
4. Interagir avec