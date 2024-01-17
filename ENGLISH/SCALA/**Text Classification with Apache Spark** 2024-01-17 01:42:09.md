```scala
// Import the necessary libraries
import org.apache.spark.sql.{DataFrame, SparkSession}
import org.apache.spark.ml.feature.{HashingTF, IDF, Tokenizer}
import org.apache.spark.ml.classification.{LogisticRegression, LogisticRegressionModel}
import org.apache.spark.ml.Pipeline
import org.apache.spark.ml.evaluation.BinaryClassificationEvaluator

// Create a SparkSession
val spark = SparkSession.builder().appName("Text Classification").master("local[*]").getOrCreate()

// Load the training data
val trainingData = spark.read.option("header", "true").option("inferSchema", "true").csv("train.csv")

// Preprocess the data
val tokenizer = new Tokenizer().setInputCol("text").setOutputCol("words")
val hashingTF = new HashingTF().setInputCol("words").setOutputCol("features")
val idf = new IDF().setInputCol("features").setOutputCol("tfidf")

// Create a pipeline
val pipeline = new Pipeline().setStages(Array(tokenizer, hashingTF, idf))

// Fit the pipeline to the training data
val model = pipeline.fit(trainingData)

// Load the test data
val testData = spark.read.option("header", "true").option("inferSchema", "true").csv("test.csv")

// Transform the test data using the pipeline
val transformedTestData = model.transform(testData)

// Extract the prediction and actual labels
val predictionsAndLabels = transformedTestData.select("prediction", "label")

// Evaluate the model
val evaluator = new BinaryClassificationEvaluator().setLabelCol("label").setRawPredictionCol("prediction")
val accuracy = evaluator.evaluate(predictionsAndLabels)

// Print the accuracy
println(s"Accuracy: ${accuracy}")

// Save the model
model.write.overwrite().save("text_classification_model")
```

Explanation:

This Scala code demonstrates a complete text classification pipeline using Apache Spark. It includes data loading, preprocessing, model training, evaluation, and saving. Here's a step-by-step explanation:

1. **Data Loading:**
   - `trainingData` and `testData` are loaded from CSV files using `spark.read`. The `header` and `inferSchema` options are set to automatically infer schema from the data.

2. **Data Preprocessing:**
   - Tokenization: The `Tokenizer` transforms the input text into an array of words.
   - Feature Extraction: The `HashingTF` converts each word into a feature vector using the hashing trick.
   - Feature Normalization: The `IDF` normalizes the feature vectors using the Inverse Document Frequency (IDF) technique.

3. **Pipeline Creation:**
   - A `Pipeline` is created to combine the preprocessing steps into a single workflow. The pipeline consists of the `Tokenizer`, `HashingTF`, and `IDF` stages.

4. **Model Training:**
   - The `LogisticRegression` model is used for text classification. The `PipelineModel` is fitted on the training data using the `fit` method.

5. **Model Evaluation:**
   - The transformed test data is obtained by applying the pipeline on the test data.
   - The `BinaryClassificationEvaluator` is used to calculate the accuracy of the model.

6. **Model Saving:**
   - The trained model is saved to a file using the `write` method. This allows the model to be reused later without the need for retraining.