**Système de Gestion de Base de Données Relationnelle en TCL (SGBDR)**

```tcl
package require Tcl 8.5
package require sqlite3

proc connect {dbName} {
    global appData
    set con [load sqlite3::database $dbName]
    if {[sqlite3::exec $con "PRAGMA synchronous = OFF"] == {}} {
        appData(db) set $con
        return 1
    }
    return 0
}

proc disconnect {} {
    global appData
    uplevel 1 $appData(db) delete
    return 1
}

proc executeQuery {query} {
    global appData
    set result [sqlite3::exec [appData(db)] $query]
    return $result
}

proc executeNonQuery {query} {
    global appData
    uplevel 1 $appData(db) set rowsChanged [sqlite3::exec [appData(db)] $query]
    return $rowsChanged
}

proc beginTransaction {} {
    global appData
    sqlite3::exec [appData(db)] "BEGIN TRANSACTION"
    return 1
}

proc rollbackTransaction {} {
    global appData
    sqlite3::exec [appData(db)] "ROLLBACK TRANSACTION"
    return 1
}

proc commitTransaction {} {
    global appData
    sqlite3::exec [appData(db)] "COMMIT TRANSACTION"
    return 1
}

proc prepareStatement {statement} {
    global appData
    set stmt [sqlite3::prepare [appData(db)] $statement]
    return $stmt
}

proc executePreparedQuery {stmt bindVars} {
    set result [sqlite3::execute $stmt $bindVars]
    return $result
}

proc executePreparedNonQuery {stmt bindVars} {
    set rowsChanged [sqlite3::execute $stmt $bindVars]
    return $rowsChanged
}

proc columnCount {resultSet} {
    return [sqlite3::column_count $resultSet]
}

proc columnName {resultSet index} {
    return [sqlite3::column_name $resultSet $index]
}

proc columnType {resultSet index} {
    return [sqlite3::column_type $resultSet $index]
}

proc columnValue {resultSet index} {
    return [sqlite3::column_value $resultSet $index]
}

proc step {resultSet} {
    return [sqlite3::step $resultSet]
}

proc finalize {stmt} {
    sqlite3::finalize $stmt
    return 1
}

```

**Utilisation du SGBDR**

```tcl
connect "ma_base.db"

beginTransaction
executeUpdate "INSERT INTO clients (nom, prenom, email) VALUES ('Dupont', 'Jean', 'jean.dupont@exemple.fr')"
executeUpdate "INSERT INTO clients (nom, prenom, email) VALUES ('Martin', 'Julie', 'julie.martin@exemple.fr')"
commitTransaction

set rs [executeQuery "SELECT * FROM clients"]
while {[step $rs]} {
    puts [columnValue $rs 0]
    puts [columnValue $rs 1]
    puts [columnValue $rs 2]
}
finalize $rs
disconnect
```

**Explication**

Ce code crée un SGBDR simplifié en TCL en utilisant la bibliothèque SQLite 3. Il fournit des fonctions pour se connecter et se déconnecter à une base de données, exécuter des requêtes, préparer des déclarations et itérer sur les résultats. Il prend en charge les transactions pour garantir l'intégrité des données.

Les fonctions `columnName()` et `columnType()` permettent de récupérer des métadonnées sur les colonnes d'un résultat. La fonction `columnValue()` renvoie la valeur d'une colonne à un index donné.

La fonction `step()` avance le curseur de résultat vers l'enregistrement suivant et renvoie 1 si un enregistrement est disponible. La fonction `finalize()` ferme une déclaration préparée et libère les ressources associées.