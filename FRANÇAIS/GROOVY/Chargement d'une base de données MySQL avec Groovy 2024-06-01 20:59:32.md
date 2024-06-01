**Charger une base de données MySQL**

```groovy
def url = "jdbc:mysql://localhost:3306/ma_base_de_donnees"
def user = "utilisateur"
def password = "mon_mot_de_passe"
def driver = "com.mysql.cj.jdbc.Driver"

// Configure la connexion à la base de données
def connection = DriverManager.getConnection(url, user, password)
```

**Créer une table et insérer des données**

```groovy
// Préparation de l'instruction SQL de création de table
def createTableSql = "CREATE TABLE si_ce_n_existe_pas (id INT NOT NULL AUTO_INCREMENT, nom VARCHAR(255), age INT, PRIMARY KEY (id))"

def statement = connection.createStatement()
// Exécution de la requête de création de table
statement.executeUpdate(createTableSql)

// Préparation de l'instruction SQL d'insertion de données
def insertSql = "INSERT INTO si_ce_n_existe_pas (nom, age) VALUES (?, ?)"

def preparedStatement = connection.prepareStatement(insertSql)
preparedStatement.setString(1, "John")
preparedStatement.setInt(2, 30)
// Exécution de la requête d'insertion de données
preparedStatement.executeUpdate()

```

**Récupérer des données**

```groovy
// Préparation de l'instruction SQL de sélection de données
def selectSql = "SELECT * FROM si_ce_n_existe_pas"

// Récupération des résultats de la requête
def resultSet = statement.executeQuery(selectSql)

// Boucle sur les lignes de résultats
while (resultSet.next()) {
    def id = resultSet.getInt("id")
    def nom = resultSet.getString("nom")
    def age = resultSet.getInt("age")
    println("Id : ${id}, Nom : ${nom}, Age : ${age}")
}
```
