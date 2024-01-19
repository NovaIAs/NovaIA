```scala
// Import necessary libraries
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.ml.feature.{HashingTF, IDF, Tokenizer}
import org.apache.spark.ml.classification.LogisticRegression
import org.apache.spark.ml.Pipeline

// Initialize SparkSession
val spark = SparkSession.builder().appName("Text Sentiment Classification").getOrCreate()

// Load the training data
val trainingData = spark.read.csv("path/to/training_data.csv")

// Tokenize the text data
val tokenizer = new Tokenizer().setInputCol("text").setOutputCol("words")
val tokenizedData = tokenizer.transform(trainingData)

// Apply hashing to convert words into numerical features
val hashingTF = new HashingTF().setInputCol("words").setOutputCol("features")
val featurizedData = hashingTF.transform(tokenizedData)

// Compute TF-IDF features
val idf = new IDF().setInputCol("features").setOutputCol("tfidf_features")
val tfidfFeaturesData = idf.fit(featurizedData).transform(featurizedData)

// Split the data into training and testing sets
val splitData = tfidfFeaturesData.randomSplit(Array(0.7, 0.3), seed = 1234L)
val trainingSet = splitData(0)
val testSet = splitData(1)

// Create a logistic regression model
val lr = new LogisticRegression().setMaxIter(10).setRegParam(0.01)

// Define a pipeline comprising the entire workflow
val pipeline = new Pipeline().setStages(Array(tokenizer, hashingTF, idf, lr))

// Fit the pipeline to the training data
val fittedPipelineModel = pipeline.fit(trainingSet)

// Evaluate the model on the test data
val testResults = fittedPipelineModel.transform(testSet)
val accuracy = testResults.filter("label == prediction").count() / testResults.count().toDouble

// Print the accuracy
println(s"Test Accuracy: ${accuracy * 100}%")

// Saving the model
fittedPipelineModel.write.overwrite().save("path/to/trained_sentiment_model")
```

**Explanation:**

1. **Data Loading:** Load the training data from a CSV file using `spark.read.csv`.


2. **Tokenization:** Split the text data into individual words using the `Tokenizer` transformer.


3. **Hashing:** Convert the words into numerical features using the `HashingTF` transformer.


4. **TF-IDF Computation:** Calculate TF-IDF features using the `IDF` transformer.


5. **Data Splitting:** Split the data into training and testing sets using `randomSplit`.


6. **Logistic Regression Model:** Initialize a logistic regression model using `LogisticRegression`.


7. **Pipeline Creation:** Create a pipeline of the entire workflow, including tokenization, hashing, TF-IDF computation, and logistic regression.


8. **Model Training:** Fit the pipeline to the training data using `fit`.


9. **Model Evaluation:** Evaluate the model on the test data using `transform`. Calculate the accuracy of the model by comparing predicted labels with actual labels.


10. **Model Saving:** Save the trained model using `write.overwrite.save`.

This code demonstrates a complex text sentiment classification pipeline using Spark MLlib. From data loading and preprocessing to model training and evaluation, it provides a detailed example of building and assessing a machine learning model in Scala.