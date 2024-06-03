**Programme de simulation d'une épidémie**

**Objectif:** Simuler la propagation d'une épidémie dans une population donnée en tenant compte des différents facteurs d'influence (taux de transmission, période d'incubation, durée de la maladie, taux de mortalité).

**Variables:**

* `population`: Nombre d'individus dans la population
* `taux_transmission`: Taux de transmission de la maladie
* `periode_incubation`: Période d'incubation de la maladie (en nombre de jours)
* `duree_maladie`: Durée moyenne de la maladie (en nombre de jours)
* `taux_mortalite`: Taux de mortalité de la maladie
* `nb_jours_simulation`: Nombre de jours de simulation
* `individus`: Liste des individus de la population, chacun représenté par un dictionnaire contenant les informations suivantes :
    * `statut`: Indique l'état de l'individu (sain, incubateur, malade, guéri, mort)
    * `duree_incubation_restante`: Nombre de jours restants avant que l'individu devienne malade
    * `duree_maladie_restante`: Nombre de jours restants avant que l'individu guérisse
* `malades`: Liste des individus actuellement malades
* `morts`: Liste des individus décédés
* `gueris`: Liste des individus guéris

**Algorithme:**

1. **Initialisation:**
    * Créer une population d'individus sains.
    * Initialiser les variables `taux_transmission`, `periode_incubation`, `duree_maladie`, `taux_mortalite` et `nb_jours_simulation`.

2. **Simulation:**
    * Pour chaque jour de simulation :
        * **Infection:**
            * Pour chaque individu sain :
                * Générer un nombre aléatoire entre 0 et 1.
                * Si le nombre aléatoire est inférieur au `taux_transmission` et qu'il y a au moins un individu malade dans la population, l'individu sain devient incubateur.
        * **Incubation:**
            * Pour chaque individu incubateur :
                * Décrémenter la `duree_incubation_restante` de 1.
                * Si la `duree_incubation_restante` atteint 0, l'individu devient malade.
        * **Maladie:**
            * Pour chaque individu malade :
                * Décrémenter la `duree_maladie_restante` de 1.
                * Si la `duree_maladie_restante` atteint 0, l'individu guérit ou meurt.
                * Générer un nombre aléatoire entre 0 et 1.
                * Si le nombre aléatoire est inférieur au `taux_mortalite`, l'individu meurt. Sinon, il guérit.
        * **Fin de simulation:** Si tous les individus sont guéris ou morts, arrêter la simulation.

3. **Affichage des résultats:**
    * Afficher le nombre d'individus sains, incubateurs, malades, guéris et morts à la fin de la simulation.
    * Tracer l'évolution du nombre d'individus dans chaque état au cours de la simulation.

**Exemple d'utilisation:**

```
population = 1000
taux_transmission = 0.2
periode_incubation = 5
duree_maladie = 10
taux_mortalite = 0.05
nb_jours_simulation = 100

epidemie = SimulationEpidemie(population, taux_transmission, periode_incubation, duree_maladie, taux_mortalite, nb_jours_simulation)
epidemie.simuler()
epidemie.afficher_resultats()
```