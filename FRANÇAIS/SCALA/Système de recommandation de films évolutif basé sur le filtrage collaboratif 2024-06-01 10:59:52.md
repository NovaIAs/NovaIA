**Système de recommandation de films efficace et évolutif**

Ce code Scala met en œuvre un système de recommandation de films efficace et évolutif. Il utilise des techniques de filtrage collaboratif pour générer des recommandations personnalisées pour les utilisateurs.

```scala
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, SparkSession}

object MovieRecommender {

  // Fonction principale pour charger les données, construire le modèle et générer des recommandations
  def main(args: Array[String]): Unit = {

    // Initialiser la session Spark
    val spark = SparkSession.builder().appName("Movie Recommender").master("local").getOrCreate()
    val sc = spark.sparkContext

    // Charger les données de notation des films
    val ratingsDF: DataFrame = spark.read.csv("ratings.csv").toDF("userId", "movieId", "rating")

    // Convertir en RDD pour le traitement
    val ratingsRDD: RDD[(Int, Int, Double)] = ratingsDF.rdd.map(row => (row(0).asInstanceOf[Int], row(1).asInstanceOf[Int], row(2).asInstanceOf[Double]))

    // Construire la matrice de similarité des utilisateurs en utilisant le filtrage collaboratif
    val userSimilarityMatrix: RDD[(Int, Array[(Int, Double)])] = MovieRecommender.computeUserSimilarityMatrix(ratingsRDD)

    // Construire la matrice de similarité des films en utilisant le filtrage collaboratif
    val movieSimilarityMatrix: RDD[(Int, Array[(Int, Double)])] = MovieRecommender.computeMovieSimilarityMatrix(ratingsRDD)

    // Générer des recommandations pour chaque utilisateur
    val userRecommendations: RDD[(Int, List[(Int, Double)])] = MovieRecommender.generateUserRecommendations(ratingsRDD, userSimilarityMatrix, movieSimilarityMatrix)

    // Sauvegarder les recommandations dans un fichier
    userRecommendations.coalesce(1).saveAsTextFile("recommendations.txt")

    // Arrêter la session Spark
    spark.stop()
  }

  // Construire la matrice de similarité des utilisateurs en utilisant le cosinus
  def computeUserSimilarityMatrix(ratingsRDD: RDD[(Int, Int, Double)]): RDD[(Int, Array[(Int, Double)])] = {

    // Calculer la similarité de chaque paire d'utilisateurs
    val userSimilarityPairs: RDD[(Int, Int, Double)] = ratingsRDD.map(rating => ((rating._1, rating._2), rating._3))
      .combineByKey(List(_), (list: List[Double], value: Double) => value :: list, (list1: List[Double], list2: List[Double]) => list1 ++ list2)
      .map { case ((user1, user2), ratings) => ((user1, user2), MovieRecommender.computeCosineSimilarity(ratings)) }

    // Grouper par utilisateur et trier par similarité décroissante
    val userSimilarityMatrix: RDD[(Int, Array[(Int, Double)])] = userSimilarityPairs.groupByKey().mapValues(_.toArray.sortBy(_._2).reverse.take(10))

    userSimilarityMatrix
  }

  // Construire la matrice de similarité des films en utilisant le cosinus
  def computeMovieSimilarityMatrix(ratingsRDD: RDD[(Int, Int, Double)]): RDD[(Int, Array[(Int, Double)])] = {

    // Calculer la similarité de chaque paire de films
    val movieSimilarityPairs: RDD[(Int, Int, Double)] = ratingsRDD.map(rating => ((rating._1, rating._2), rating._3))
      .combineByKey(List(_), (list: List[Double], value: Double) => value :: list, (list1: List[Double], list2: List[Double]) => list1 ++ list2)
      .map { case ((user1, movie1), ratings) => ((user1, movie1), MovieRecommender.computeCosineSimilarity(ratings)) }

    // Grouper par film et trier par similarité décroissante
    val movieSimilarityMatrix: RDD[(Int, Array[(Int, Double)])] = movieSimilarityPairs.groupByKey().mapValues(_.toArray.sortBy(_._2).reverse.take(10))

    movieSimilarityMatrix
  }

  // Générer des recommandations pour chaque utilisateur
  def generateUserRecommendations(ratingsRDD: RDD[(Int, Int, Double)], userSimilarityMatrix: RDD[(Int, Array[(Int, Double)])], movieSimilarityMatrix: RDD[(Int, Array[(Int, Double)])]): RDD[(Int, List[(Int, Double)])] = {

    // Rejoindre les matrices de similarité des utilisateurs et des films
    val joinedSimilarityMatrix: RDD[(Int, (Array[(Int, Double)], Array[(Int, Double)]))] = userSimilarityMatrix.join(movieSimilarityMatrix)

    // Calculer les scores de recommandation pour chaque film
    val userRecommendations: RDD[(Int, List[(Int, Double)])] = ratingsRDD.map(rating => (rating._1, (rating._2, rating._3)))
      .reduceByKey((user1Rating, user2Rating) => (user1Rating._1, user1Rating._2 + user2Rating._2))
      .join(joinedSimilarityMatrix).map { case (userId, ((movieId, userRating), (userSimilar, movieSimilar))) =>
      (userId, MovieRecommender.calculateRecommendationScore(userSimilar, movieSimilar, movieId, userRating))
    }.groupByKey().mapValues(_.toList.sortBy(_._2).reverse.take(10))

    userRecommendations
  }

  // Calculer la similarité cosinus
  def computeCosineSimilarity(ratings: List[Double]): Double = {

    val numerator = ratings.reduceLeft(_ * _)
    val denominator = math.sqrt(ratings.map(rating => rating * rating).reduceLeft(_ + _))

    numerator / denominator
  }

  // Calculer le score de recommandation
  def calculateRecommendationScore(userSimilar: Array[(Int, Double)], movieSimilar: Array[(Int, Double)], movieId: Int, userRating: Double): (Int, Double) = {

    val similarUsers: Array[(Int, Double)] = userSimilar.filterNot(user => user._1 == movieId)
    val similarMovies: Array[(Int, Double)] = movieSimilar.filterNot(movie => movie._1 == movieId)

    val weightedUserSimilarities: Array[(Int, Double)] = similarUsers.map(user => (user._1, user._2 * userRating))
    val weightedMovieSimilarities: Array[(Int, Double)] = similarMovies.map(movie => (movie._1, movie._2 * userRating))

    val userSimilaritySum: Double = weightedUserSimilarities.map(_._2).sum
    val movieSimilaritySum: Double = weightedMovieSimilarities.map(_._2).sum

    (movieId, (weightedUserSimilarities.map(user => user._1 * user._2).sum) / userSimilaritySum + (weightedMovieSimilarities.map(movie => movie._1 * movie._2).sum) / movieSimilaritySum)
  }
}
```

**Explication du code :**

* La fonction `main` charge les données de notation des films et construit les matrices de similarité des utilisateurs et des films. Elle génère ensuite des recommandations pour chaque utilisateur et les enregistre dans un fichier.

* La fonction `computeUserSimilarityMatrix` calcule la matrice de similarité des utilisateurs à l'aide du filtrage collaboratif et de la mesure de similarité cosinus.

* La fonction `computeMovieSimilarityMatrix` calcule la matrice de similarité des films à l'aide du filtrage collaboratif et de la mesure de similarité cosinus.

* La fonction `generateUserRecommendations` génère des recommandations pour chaque utilisateur en rejoignant les matrices de similarité des utilisateurs et des films et en calculant les scores de recommandation.

* La fonction `computeCosineSimilarity` calcule la mesure de similarité cosinus.

* La fonction `calculateRecommendationScore` calcule le score de recommandation en pondérant les scores de similarité des utilisateurs et des films.