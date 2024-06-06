**Système de gestion de bibliothèque complet en Perl**

**Bibliothèques requises :**

```
use DBI;
use CGI;
use Template;
```

**Connexion à la base de données (DBI) :**

```
my $dsn = "DBI:mysql:database=bibliotheque;host=localhost";
my $user = "root";
my $pass = "mon_mot_de_passe";

my $dbh = DBI->connect($dsn, $user, $pass) or die $DBI::errstr;
```

**Interface graphique (CGI) :**

```
my $query = CGI::new;

my $action = $query->param('action');
my $id = $query->param('id');
```

**Gestion des actions de l'utilisateur (Template) :**

```
my $tmpl = Template->new(
    RELATIVE => 'templates',
    ABSOLUTE => '/chemin_absolu/vers/templates'
);

if ($action eq 'lister_livres') {
    $tmpl->process('livres.tt', { livres => $dbh->selectall_arrayref('SELECT * FROM livres') })->print;
} elsif ($action eq 'ajouter_livre') {
    $tmpl->process('ajouter_livre.tt')->print;
} elsif ($action eq 'enregistrer_livre') {
    my $titre = $query->param('titre');
    my $auteur = $query->param('auteur');

    $dbh->do("INSERT INTO livres (titre, auteur) VALUES (?, ?)", undef, $titre, $auteur);
    $tmpl->process('livre_ajoute.tt')->print;
} elsif ($action eq 'consulter_livre') {
    my $livre = $dbh->selectall_arrayref("SELECT * FROM livres WHERE id = ?", undef, $id);
    $tmpl->process('livre.tt', { livre => $livre })->print;
} elsif ($action eq 'modifier_livre') {
    my $livre = $dbh->selectall_arrayref("SELECT * FROM livres WHERE id = ?", undef, $id);
    $tmpl->process('modifier_livre.tt', { livre => $livre })->print;
} elsif ($action eq 'enregistrer_modification') {
    my $titre = $query->param('titre');
    my $auteur = $query->param('auteur');

    $dbh->do("UPDATE livres SET titre = ?, auteur = ? WHERE id = ?", undef, $titre, $auteur, $id);
    $tmpl->process('livre_modifie.tt')->print;
} elsif ($action eq 'supprimer_livre') {
    $dbh->do("DELETE FROM livres WHERE id = ?", undef, $id);
    $tmpl->process('livre_supprime.tt')->print;
}
```

**Explication du code :**

Ce code Perl crée un système de gestion de bibliothèque complet qui permet aux utilisateurs d'effectuer les opérations suivantes :

* Lister les livres
* Ajouter un nouveau livre
* Consulter les détails d'un livre
* Modifier un livre
* Supprimer un livre

Il utilise des modules Perl standard comme DBI pour gérer la connexion et les interactions avec une base de données MySQL, CGI pour gérer les requêtes HTTP et Template pour gérer l'interface graphique. Le code est modulaire et utilise des actions et des modèles pour gérer les différentes fonctions de l'application de manière organisée.