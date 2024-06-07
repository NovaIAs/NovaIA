**Gestionnaire de contact complexe et hiérarchique en C#**

Ce code gère une hiérarchie complexe de contacts avec des attributs et des relations personnalisées.

```csharp
// Classe abstraite de base pour tous les contacts
public abstract class Contact
{
    public int Id { get; set; }
    public string Nom { get; set; }
    public string Prenom { get; set; }
    public virtual ICollection<Contact> ContactsLiés { get; set; } = new HashSet<Contact>();
}

// Classe pour les contacts individuels
public class ContactIndividuel : Contact
{
    public string Adresse { get; set; }
    public string Téléphone { get; set; }
    public string Email { get; set; }
}

// Classe pour les groupes de contacts
public class GroupeContact : Contact
{
    public virtual ICollection<Contact> Membres { get; set; } = new HashSet<Contact>();
}

// Classe pour les organisations
public class Organisation : Contact
{
    public string NomOrganisation { get; set; }
    public string AdresseOrganisation { get; set; }
    public string TéléphoneOrganisation { get; set; }
}

// Classe pour hiérarchiser les contacts
public class HiérarchieContact
{
    public int Niveau { get; set; }
    public Contact ContactParent { get; set; }
    public virtual ICollection<Contact> ContactsEnfants { get; set; } = new HashSet<Contact>();
}

// Classe gérant les relations entre les contacts
public class RelationContact
{
    public int TypeRelation { get; set; }
    public Contact Contact1 { get; set; }
    public Contact Contact2 { get; set; }
}

// Classe de contexte de données EF
public class ContexteDonneesContact : DbContext
{
    public DbSet<ContactIndividuel> ContactsIndividuels { get; set; }
    public DbSet<GroupeContact> GroupesContact { get; set; }
    public DbSet<Organisation> Organisations { get; set; }
    public DbSet<HiérarchieContact> HiérarchiesContact { get; set; }
    public DbSet<RelationContact> RelationsContact { get; set; }
}

// Programme principal
class Program
{
    static void Main(string[] args)
    {
        // Création du contexte de données
        using (var contexte = new ContexteDonneesContact())
        {
            // Création d'un contact individuel
            var contactIndividuel = new ContactIndividuel
            {
                Nom = "Dupont",
                Prenom = "Jean",
                Adresse = "1 rue des Champs",
                Téléphone = "01 23 45 67 89",
                Email = "jean.dupont@gmail.com"
            };

            // Création d'un groupe de contacts
            var groupeContact = new GroupeContact
            {
                Nom = "Famille Dupont",
                Membres = { contactIndividuel }
            };

            // Création d'une organisation
            var organisation = new Organisation
            {
                NomOrganisation = "Société XYZ",
                AdresseOrganisation = "10 avenue des Entrepreneurs",
                TéléphoneOrganisation = "02 34 56 78 90"
            };

            // Création d'une hiérarchie de contacts
            var hiérarchieContact = new HiérarchieContact
            {
                Niveau = 1,
                ContactParent = null,
                ContactsEnfants = { groupeContact, organisation }
            };

            // Ajout des objets au contexte de données
            contexte.ContactsIndividuels.Add(contactIndividuel);
            contexte.GroupesContact.Add(groupeContact);
            contexte.Organisations.Add(organisation);
            contexte.HiérarchiesContact.Add(hiérarchieContact);

            // Sauvegarde des modifications
            contexte.SaveChanges();
        }
    }
}
```

**Explication du code:**

* **Classes de contact:** `Contact`, `ContactIndividuel`, `GroupeContact` et `Organisation` représentent la hiérarchie des contacts.
* **HiérarchieContact:** Gère la structure hiérarchique des contacts.
* **RelationContact:** Modélise les relations entre les contacts.
* **Contexte de données EF:** Gestion de la base de données et du mappage objet-relationnel.
* **Main:** Exemple d'utilisation de la hiérarchie pour créer et enregistrer des contacts.