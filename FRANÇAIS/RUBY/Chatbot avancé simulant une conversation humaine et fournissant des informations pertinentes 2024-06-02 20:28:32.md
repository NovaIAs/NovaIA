**Objectif :** Créer un chatbot avancé qui simule une conversation humaine et fournit des informations pertinentes.

**Code :**

```ruby
require 'json'
require 'open-uri'
require 'cgi'
require 'tk'

# Chargons le fichier JSON des intentions et des réponses.
intentions = JSON.parse(open('intentions.json').read)

# Créons la fenêtre principale du chatbot.
fenetre = TkRoot.new
fenetre.title = "Chatbot"

# Champ de saisie de l'utilisateur.
champ_saisie = TkEntry.new(fenetre)
champ_saisie.pack

# Champ d'affichage des réponses.
champ_reponse = TkText.new(fenetre)
champ_reponse.pack

# Définition de la fonction de traitement du texte de l'utilisateur.
def traite_texte(texte)
  # Suppression des espaces superflus.
  texte = texte.strip

  # Normalisation du texte en minuscules.
  texte = texte.downcase

  # Retour du texte traité.
  return texte
end

# Définition de la fonction de génération des réponses.
def genere_reponses(texte)
  # Parcours des intentions.
  intentions.each do |intention|
    # Vérification de la correspondance avec l'intention.
    if texte.include? intention["mots_cles"]
      # Sélection des réponses aléatoires.
      reponses = intention["reponses"].shuffle.take(2)

      # Retour des réponses.
      return reponses
    end
  end

  # Si aucune correspondance n'est trouvée, renvoie une réponse par défaut.
  ["Je ne suis pas sûr de ce que vous demandez.", "Pouvez-vous s'il vous plaît reformuler votre question ?"]
end

# Définition de l'événement d'appui sur la touche Entrée.
champ_saisie.bind("Return") do
  # Récupération du texte saisi par l'utilisateur.
  texte_utilisateur = champ_saisie.get

  # Traitement du texte.
  texte_traite = traite_texte(texte_utilisateur)

  # Génération des réponses.
  reponses = genere_reponses(texte_traite)

  # Affichage des réponses.
  champ_reponse.insert("end", "\n")
  reponses.each do |reponse|
    champ_reponse.insert("end", reponse + "\n")
  end

  # Effacement du champ de saisie.
  champ_saisie.delete(0, "end")
end

# Démarrage de la boucle principale de l'interface graphique.
Tk.mainloop
```

**Explication :**

* Le chatbot charge un fichier JSON contenant les intentions et les réponses possibles.
* Il dispose d'une fenêtre avec un champ de saisie pour l'utilisateur et un champ d'affichage pour les réponses.
* Lorsque l'utilisateur appuie sur la touche Entrée, le texte saisi est traité pour supprimer les espaces superflus et normalisé en minuscules.
* Le texte traité est comparé aux intentions définies dans le fichier JSON.
* Si une correspondance est trouvée, des réponses aléatoires sont sélectionnées dans l'intention correspondante et affichées.
* Si aucune correspondance n'est trouvée, une réponse par défaut est affichée.