```ruby
# Initialisation des variables
nombre_de_tours = 10
score_joueur_1 = 0
score_joueur_2 = 0
compteur = 1

# Boucle principale du jeu
while compteur <= nombre_de_tours do
  puts "Tour #{compteur}"
  puts "Joueur 1, entrez un nombre entre 1 et 100 :"
  nombre_1 = gets.chomp.to_i

  puts "Joueur 2, entrez un nombre entre 1 et 100 :"
  nombre_2 = gets.chomp.to_i

  # Calcul des scores
  if nombre_1 > nombre_2
    score_joueur_1 += 1
  elsif nombre_1 < nombre_2
    score_joueur_2 += 1
  end

  # Affichage des scores
  puts "Score du joueur 1 : #{score_joueur_1}"
  puts "Score du joueur 2 : #{score_joueur_2}"

  # Incrémentation du compteur
  compteur += 1
end

# Déclaration du gagnant
if score_joueur_1 > score_joueur_2
  puts "Le joueur 1 a gagné !"
elsif score_joueur_1 < score_joueur_2
  puts "Le joueur 2 a gagné !"
else
  puts "Match nul !"
end
```

**Explication du code :**

* **Initialisation des variables** : Les variables `nombre_de_tours`, `score_joueur_1`, `score_joueur_2` et `compteur` sont initialisées avec leurs valeurs respectives.
* **Boucle principale** : La boucle `while` s'exécute tant que `compteur` est inférieur ou égal à `nombre_de_tours`. À chaque tour, elle fait entrer les nombres des joueurs et calcule les scores en fonction du plus grand.
* **Calcul des scores** : Si le nombre du joueur 1 est supérieur à celui du joueur 2, le score du joueur 1 est incrémenté de 1. Si le nombre du joueur 1 est inférieur à celui du joueur 2, le score du joueur 2 est incrémenté de 1.
* **Affichage des scores** : Les scores des deux joueurs sont affichés à chaque tour.
* **Déclaration du gagnant** : Après la boucle, une condition `if...elsif...else` détermine le gagnant en fonction des scores.