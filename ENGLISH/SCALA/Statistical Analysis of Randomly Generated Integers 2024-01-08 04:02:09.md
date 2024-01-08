```scala
import scala.util.Random

object ComplexScalaCode {

  // Function to generate a random list of integers of a given length
  def generateRandomList(length: Int): List[Int] = {
    val random = new Random()
    (1 to length).map(_ => random.nextInt(1000)).toList
  }

  // Function to find the maximum value in a list of integers
  def findMax(list: List[Int]): Int = {
    list.max
  }

  // Function to find the minimum value in a list of integers
  def findMin(list: List[Int]): Int = {
    list.min
  }

  // Function to calculate the average of a list of integers
  def calculateAverage(list: List[Int]): Double = {
    list.sum.toDouble / list.length
  }

  // Function to calculate the standard deviation of a list of integers
  def calculateStandardDeviation(list: List[Int]): Double = {
    val mean = calculateAverage(list)
    val variances = list.map(x => math.pow(x - mean, 2))
    math.sqrt(variances.sum / list.length)
  }

  // Function to find the mode of a list of integers
  def findMode(list: List[Int]): Int = {
    val frequencyMap = list.groupBy(x => x).mapValues(_.length)
    val maxFrequency = frequencyMap.values.max
    frequencyMap.filter(_._2 == maxFrequency).keys.min
  }

  // Function to find the median of a list of integers
  def findMedian(list: List[Int]): Double = {
    val sortedList = list.sorted
    val midIndex = sortedList.length / 2
    if (sortedList.length % 2 == 0) {
      (sortedList(midIndex) + sortedList(midIndex - 1)) / 2.0
    } else {
      sortedList(midIndex)
    }
  }

  // Function to find the quartiles of a list of integers
  def findQuartiles(list: List[Int]): (Double, Double, Double) = {
    val sortedList = list.sorted
    val q1Index = sortedList.length / 4
    val q2Index = sortedList.length / 2
    val q3Index = sortedList.length * 3 / 4
    (findMedian(sortedList.take(q1Index)), findMedian(sortedList), findMedian(sortedList.drop(q3Index)))
  }

  // Function to find the outliers of a list of integers
  def findOutliers(list: List[Int], iqr: Double): List[Int] = {
    val lowerBound = findMedian(list) - (1.5 * iqr)
    val upperBound = findMedian(list) + (1.5 * iqr)
    list.filter(x => x < lowerBound || x > upperBound)
  }

  def main(args: Array[String]): Unit = {
    val list = generateRandomList(1000)

    println("Maximum value: " + findMax(list))
    println("Minimum value: " + findMin(list))
    println("Average value: " + calculateAverage(list))
    println("Standard deviation: " + calculateStandardDeviation(list))
    println("Mode: " + findMode(list))
    println("Median: " + findMedian(list))
    println("Quartiles: " + findQuartiles(list))
    println("Outliers: " + findOutliers(list, calculateStandardDeviation(list)))
  }
}
```

Explanation:

This Scala code performs various statistical operations on a list of randomly generated integers. Here's what each part of the code does:

1. `generateRandomList` Function:
   - This function generates a list of random integers of a specified length. It uses the `Random` class to generate random numbers and creates a list of these random numbers.

2. `findMax` Function:
   - This function finds the maximum value in a list of integers. It uses the `max` method on the list to find the largest number.

3. `findMin` Function:
   - This function finds the minimum value in a list of integers. It uses the `min` method on the list to find the smallest number.

4. `calculateAverage` Function:
   - This function calculates the average of a list of integers. It sums up all the values in the list and divides it by the length of the list to get the average.

5. `calculateStandardDeviation` Function:
   - This function calculates the standard deviation of a list of integers. It first calculates the mean (average) of the list, then finds the variance (the squared difference between each value and the mean), and finally takes the square root of the average variance to get the standard deviation.

6. `findMode` Function:
   - This function finds the mode (the most frequently occurring value) of a list of integers. It groups the values in the list by their value and counts the number of occurrences of each value. The value with the highest occurrence is the mode.

7. `findMedian` Function:
   - This function finds the median (the middle value) of a list of integers. It first sorts the list in ascending order and then finds the middle value. If the list has an even number of elements, the median is the average of the two middle values.

8. `findQuartiles` Function:
   - This function finds the quartiles (the 25th, 50th, and 75th percentiles) of a list of integers. It first sorts the list and then finds the median of the lower half (25th percentile), the median of the entire list (50th percentile), and the median of the upper half (75th percentile).

9. `findOutliers` Function:
   - This function finds the outliers in a list of integers. It first calculates the interquartile range (IQR), which is the difference between the 75th and 25th percentiles. Then, it finds the lower and upper bounds for outliers as 1.5 times the IQR below and above the median, respectively. Any values outside these bounds are considered outliers.

10. `main` Function:
    - This is the entry point of the program. It generates a random list of 1000 integers, calls the various statistical functions to calculate different statistics on the list, and prints the results.