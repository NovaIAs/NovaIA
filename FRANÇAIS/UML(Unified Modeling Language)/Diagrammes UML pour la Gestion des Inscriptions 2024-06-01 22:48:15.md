**Diagramme de Classes**

**Classes Abstraites**

* **Entité**
    * **Identifiant** : Int
    * **Nom** : String
    * **Description** : String

**Classes Concrètes**

* **Étudiant** (hérite de **Entité**)
    * **NuméroÉtudiant** : Int
    * **Adresse** : String
* **Enseignant** (hérite de **Entité**)
    * **NuméroEnseignant** : Int
    * **Matière** : String
* **Cours** (hérite de **Entité**)
    * **NomCours** : String
    * **Crédits** : Int
* **Inscription** (hérite de **Entité**)
    * **Étudiant** : Étudiant
    * **Cours** : Cours
    * **Note** : Int

**Relations**

* **Étudiant** **1..* **Inscription**
* **Cours** **1..* **Inscription**
* **Enseignant** **1..* **Cours**

**Diagramme de Séquences**

* **Inscrire un étudiant à un cours**

**Participants**

* Étudiant
* Cours
* Inscription

**Messages**

* **Étudiant** -> **Cours** : Demande d'inscription
* **Cours** -> **Inscription** : Création de l'inscription
* **Inscription** -> **Étudiant** : Confirmation d'inscription

**Diagramme de Cas d'Utilisation**

**Cas d'utilisation** : **Gérer les inscriptions aux cours**

**Acteur** : Étudiant

**Prérequis** : L'étudiant est inscrit dans le système.

**Déroulement** :

1. L'étudiant sélectionne un cours.
2. Le système vérifie si l'étudiant peut s'inscrire au cours.
3. Si l'étudiant peut s'inscrire, le système crée une inscription.
4. Le système affiche une confirmation d'inscription à l'étudiant.

**Cas d'exception** :

* L'étudiant n'est pas inscrit dans le système.
* L'étudiant ne peut pas s'inscrire au cours (par exemple, prérequis manquants).

**Diagramme d'Etats-Transitions**

**Machine à états** : **Inscription**

**États**

* **Créé**
* **Confirmé**
* **Annulé**

**Transitions**

* **Créé** -> **Confirmé** : L'inscription est confirmée.
* **Confirmé** -> **Annulé** : L'inscription est annulée.

**Diagramme de Déploiement**

* **Serveur d'applications** :
    * **Composant** : Système de gestion des inscriptions aux cours
    * **Dépendances** : Base de données, serveur Web
* **Base de données** :
    * **Composant** : Base de données relationnelle
* **Serveur Web** :
    * **Composant** : Serveur Apache
    * **Dépendances** : Navigateur Web

**Diagramme d'Activités**

**Processus** : **Calculer la moyenne d'un étudiant**

**Activités**

* **Obtenir les inscriptions de l'étudiant**
* **Pour chaque inscription**
    * **Obtenir la note de l'inscription**
    * **Additionner la note à la somme totale**
* **Calculer la moyenne en divisant la somme totale par le nombre d'inscriptions**