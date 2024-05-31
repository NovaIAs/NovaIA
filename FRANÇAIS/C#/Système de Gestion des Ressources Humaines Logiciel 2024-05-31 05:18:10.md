**Système de Gestion des Ressources Humaines (SRH)**

Ce code complexe en C# implémente un système de gestion des ressources humaines complet, offrant une large gamme de fonctionnalités pour gérer les informations sur les employés, les postes, les départements et les projets.

**Structure des Classes**

Le code utilise une architecture orientée objet avec les classes suivantes :

* **Employe** : Représente les informations sur les employés, y compris les détails personnels, les compétences et l'historique d'emploi.
* **Poste** : Représente les différents postes au sein de l'entreprise, avec des descriptions de poste et des exigences.
* **Département** : Représente les divisions organisationnelles de l'entreprise, avec des hiérarchies et des structures de gestion.
* **Projet** : Représente les projets en cours de l'entreprise, avec des informations sur les objectifs, les échéances et les membres de l'équipe.

**Fonctions Principales**

Le système SRH fournit les fonctions suivantes :

* **Gestion des employés** : Ajouter, modifier, supprimer et rechercher des employés, gérer leurs informations personnelles, leurs compétences et leur historique d'emploi.
* **Gestion des postes** : Ajouter, modifier, supprimer et rechercher des postes, définir des descriptions de poste et des exigences.
* **Gestion des départements** : Ajouter, modifier, supprimer et rechercher des départements, créer des hiérarchies et affecter des gestionnaires.
* **Gestion des projets** : Ajouter, modifier, supprimer et rechercher des projets, définir des objectifs, des échéances et affecter des membres de l'équipe.
* **Attribution des projets** : Affecter des employés à des projets en fonction de leurs compétences et de leurs disponibilités.
* **Rapports et Analyses** : Générer des rapports sur les informations sur les employés, les postes, les départements et les projets pour la prise de décision.

**Architecture de la Base de Données**

Le système SRH utilise une base de données relationnelle pour stocker les données sur les employés, les postes, les départements et les projets. Les tables suivantes sont créées :

* **Employes** : Stocke les informations sur les employés.
* **Postes** : Stocke les informations sur les postes.
* **Départements** : Stocke les informations sur les départements.
* **Projets** : Stocke les informations sur les projets.
* **Affectations** : Stocke les affectations des employés aux projets.

**Interface Utilisateur**

Une interface utilisateur graphique (GUI) est développée à l'aide de Windows Forms pour permettre aux utilisateurs d'interagir avec le système SRH. L'interface utilisateur comprend des formulaires pour ajouter, modifier, supprimer et rechercher des informations sur les employés, les postes, les départements et les projets.

**Code Exemple**

Voici un extrait de code qui illustre la fonction d'ajout d'employé :

```c#
using System;
using System.Data;
using System.Windows.Forms;

namespace HRMS
{
    public partial class frmAddEmployee : Form
    {
        public frmAddEmployee()
        {
            InitializeComponent();
        }

        private void btnAdd_Click(object sender, EventArgs e)
        {
            try
            {
                // Créer un nouvel objet employé.
                Employee employee = new Employee();

                // Remplir l'objet employé avec les données saisies.
                employee.FirstName = txtFirstName.Text;
                employee.LastName = txtLastName.Text;
                employee.Email = txtEmail.Text;

                // Créer une connexion à la base de données.
                using (var connection = new SqlConnection(ConnectionString))
                {
                    // Ouvrir la connexion.
                    connection.Open();

                    // Créer une commande pour insérer le nouvel employé.
                    using (var command = connection.CreateCommand())
                    {
                        command.CommandText = "INSERT INTO Employees (FirstName, LastName, Email) VALUES (@FirstName, @LastName, @Email)";

                        // Ajouter les paramètres de la commande.
                        command.Parameters.AddWithValue("@FirstName", employee.FirstName);
                        command.Parameters.AddWithValue("@LastName", employee.LastName);
                        command.Parameters.AddWithValue("@Email", employee.Email);

                        // Exécuter la commande.
                        command.ExecuteNonQuery();
                    }

                    // Fermer la connexion.
                    connection.Close();

                    // Afficher un message de succès.
                    MessageBox.Show("Employé ajouté avec succès.", "Succès", MessageBoxButtons.OK, MessageBoxIcon.Information);

                    // Fermer le formulaire.
                    this.Close();
                }
            }
            catch (Exception ex)
            {
                // Afficher un message d'erreur.
                MessageBox.Show(ex.Message, "Erreur", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}
```

Ce code gère l'événement de clic sur le bouton "Ajouter" dans le formulaire "Ajouter un employé". Il crée un nouvel objet employé, remplit les données saisies par l'utilisateur, établit une connexion à la base de données, crée une commande pour insérer le nouvel employé, exécute la commande, ferme la connexion et affiche un message de réussite.