**Création d'un Serveur Web Statique Complexe avec Serveur HTTP, Gestionnaire de Requêtes et Gestionnaire de Fichiers**

**Serveur HTTP (http-server.sh)**

```shell
#!/bin/bash

# Définir le port d'écoute (par défaut : 80)
PORT=${1:-80}

# Créer un socket d'écoute
SOCKET=$(mktemp -u -t http-server.sock)

# Fonction pour gérer les requêtes
handle_request() {
  # Lire la requête
  REQUEST=$(head -n1)
  # Extraire la méthode HTTP et le chemin d'accès
  METHOD=$(echo "$REQUEST" | cut -d ' ' -f1)
  PATH=$(echo "$REQUEST" | cut -d ' ' -f2)

  # Créer la réponse
  RESPONSE=""
  if [[ "$METHOD" == "GET" ]]; then
    # Obtenir le contenu du fichier demandé
    CONTENT=$(cat "$PATH" 2>/dev/null)
    # Si le fichier existe, envoyer le contenu
    if [[ -n "$CONTENT" ]]; then
      RESPONSE="HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n$CONTENT"
    # Sinon, envoyer une erreur 404
    else
      RESPONSE="HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\nNot Found"
    fi
  else
    # Méthode HTTP non prise en charge
    RESPONSE="HTTP/1.1 405 Method Not Allowed\r\nContent-Type: text/plain\r\n\r\nMethod Not Allowed"
  fi

  # Envoyer la réponse
  echo -e "$RESPONSE"
}

# Boucle principale
while true; do
  # Accepter une connexion
  CONN=$(nc -l -w 1 -p "$PORT" -U "$SOCKET")

  # Si une connexion est établie, gérer la requête
  if [[ -n "$CONN" ]]; then
    handle_request < <(echo "$CONN")
  fi
done
```

**Gestionnaire de Requêtes (request-handler.sh)**

```shell
#!/bin/bash

# Lire la ligne de requête
REQUEST=$(head -n1)

# Extraire la méthode HTTP et le chemin d'accès
METHOD=$(echo "$REQUEST" | cut -d ' ' -f1)
PATH=$(echo "$REQUEST" | cut -d ' ' -f2)

# Créer la réponse
RESPONSE=""
if [[ "$METHOD" == "GET" ]]; then
  # Obtenir le contenu du fichier demandé
  CONTENT=$(cat "$PATH" 2>/dev/null)
  # Si le fichier existe, envoyer le contenu
  if [[ -n "$CONTENT" ]]; then
    RESPONSE="HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n$CONTENT"
  # Sinon, envoyer une erreur 404
  else
    RESPONSE="HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\nNot Found"
  fi
else
  # Méthode HTTP non prise en charge
  RESPONSE="HTTP/1.1 405 Method Not Allowed\r\nContent-Type: text/plain\r\n\r\nMethod Not Allowed"
fi

# Envoyer la réponse
echo -e "$RESPONSE"
```

**Gestionnaire de Fichiers (file-handler.sh)**

```shell
#!/bin/bash

# Liste des fichiers et répertoires
ls -al

# Créer un répertoire
mkdir new-directory

# Créer un fichier
touch new-file

# Supprimer un fichier
rm new-file

# Copier un fichier
cp new-directory/new-file new-directory/copy-of-new-file

# Renommer un fichier
mv new-directory/copy-of-new-file new-directory/renamed-file
```

**Utilisation**

1. Exécutez le serveur HTTP :

```Bash
./http-server.sh
```

2. Connectez-vous au serveur sur le port 80 (par défaut) à l'aide de votre navigateur ou de cURL :

```Bash
curl http://localhost:80/index.html
```

3. Gérez les fichiers à l'aide du gestionnaire de fichiers :

```Bash
./file-handler.sh
```

Ce code crée un serveur Web statique qui écoute les requêtes HTTP, gère les requêtes GET et fournit un gestionnaire de fichiers avancé pour la création, la suppression, la copie et le renommage de fichiers et de répertoires.