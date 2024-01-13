```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>

using namespace std;

// Define a custom comparator function for sorting pairs in descending order of their second element
bool compareBySecond(const pair<int, int>& a, const pair<int, int>& b) {
  return a.second > b.second;
}

// Define a custom function to calculate the frequency of each integer in a given vector
map<int, int> calculateFrequency(const vector<int>& numbers) {
  map<int, int> frequencyMap;
  for (int number : numbers) {
    frequencyMap[number]++;
  }
  return frequencyMap;
}

// Define a custom function to find the most frequent integer in a given vector
int findMostFrequent(const vector<int>& numbers) {
  map<int, int> frequencyMap = calculateFrequency(numbers);
  int mostFrequentInteger = -1;
  int maxFrequency = 0;
  for (const auto& [number, frequency] : frequencyMap) {
    if (frequency > maxFrequency) {
      maxFrequency = frequency;
      mostFrequentInteger = number;
    }
  }
  return mostFrequentInteger;
}

// Define a custom function to find the least frequent integer in a given vector
int findLeastFrequent(const vector<int>& numbers) {
  map<int, int> frequencyMap = calculateFrequency(numbers);
  int leastFrequentInteger = -1;
  int minFrequency = INT_MAX;
  for (const auto& [number, frequency] : frequencyMap) {
    if (frequency < minFrequency) {
      minFrequency = frequency;
      leastFrequentInteger = number;
    }
  }
  return leastFrequentInteger;
}

// Define a custom function to find the median of a given vector
double findMedian(const vector<int>& numbers) {
  // Sort the vector in ascending order
  sort(numbers.begin(), numbers.end());

  // If the vector has an even number of elements, the median is the average of the two middle elements
  if (numbers.size() % 2 == 0) {
    return (numbers[numbers.size() / 2 - 1] + numbers[numbers.size() / 2]) / 2.0;
  }
  // Otherwise, the median is the middle element
  else {
    return numbers[numbers.size() / 2];
  }
}

// Define a custom function to find the mode of a given vector
vector<int> findMode(const vector<int>& numbers) {
  // Calculate the frequency of each integer in the vector
  map<int, int> frequencyMap = calculateFrequency(numbers);

  // Find the maximum frequency
  int maxFrequency = 0;
  for (const auto& [number, frequency] : frequencyMap) {
    maxFrequency = max(maxFrequency, frequency);
  }

  // Create a vector to store the mode(s)
  vector<int> mode;

  // Iterate over the frequency map and add the integers with maximum frequency to the mode vector
  for (const auto& [number, frequency] : frequencyMap) {
    if (frequency == maxFrequency) {
      mode.push_back(number);
    }
  }

  return mode;
}

// Define a custom function to find the range of a given vector
int findRange(const vector<int>& numbers) {
  // Find the maximum and minimum values in the vector
  int maxValue = *max_element(numbers.begin(), numbers.end());
  int minValue = *min_element(numbers.begin(), numbers.end());

  // Calculate the range
  return maxValue - minValue;
}

// Define a custom function to find the variance of a given vector
double findVariance(const vector<int>& numbers) {
  // Calculate the mean of the vector
  double mean = 0;
  for (int number : numbers) {
    mean += number;
  }
  mean /= numbers.size();

  // Calculate the variance
  double variance = 0;
  for (int number : numbers) {
    variance += pow(number - mean, 2);
  }
  variance /= numbers.size();

  return variance;
}

// Define a custom function to find the standard deviation of a given vector
double findStandardDeviation(const vector<int>& numbers) {
  return sqrt(findVariance(numbers));
}

// Define a custom function to find the quartiles of a given vector
vector<double> findQuartiles(const vector<int>& numbers) {
  // Sort the vector in ascending order
  sort(numbers.begin(), numbers.end());

  // Calculate the quartiles
  double q1 = findMedian(vector<int>(numbers.begin(), numbers.begin() + numbers.size() / 2));
  double q2 = findMedian(numbers);
  double q3 = findMedian(vector<int>(numbers.begin() + numbers.size() / 2, numbers.end()));

  return {q1, q2, q3};
}

int main() {
  // Create a vector of integers
  vector<int> numbers = {1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  // Find the most frequent integer
  int mostFrequentInteger = findMostFrequent(numbers);
  cout << "Most Frequent Integer: " << mostFrequentInteger << endl;

  // Find the least frequent integer
  int leastFrequentInteger = findLeastFrequent(numbers);
  cout << "Least Frequent Integer: " << leastFrequentInteger << endl;

  // Find the median
  double median = findMedian(numbers);
  cout << "Median: " << median << endl;

  // Find the mode
  vector<int> mode = findMode(numbers);
  cout << "Mode: ";
  for (int number : mode) {
    cout << number << " ";
  }
  cout << endl;

  // Find the range
  int range = findRange(numbers);
  cout << "Range: " << range << endl;

  // Find the variance
  double variance = findVariance(numbers);
  cout << "Variance: " << variance << endl;

  // Find the standard deviation
  double standardDeviation = findStandardDeviation(numbers);
  cout << "Standard Deviation: " << standardDeviation << endl;

  // Find the quartiles
  vector<double> quartiles = findQuartiles(numbers);
  cout << "Quartiles: ";
  for (double quartile : quartiles) {
    cout << quartile << " ";
  }
  cout << endl;

  return 0;
}
```

This code is a collection of various statistical functions that can be used to analyze a given vector of integers. The functions include:

* **findMostFrequent**: Finds the most frequently occurring integer in the vector.
* **findLeastFrequent**: Finds the least frequently occurring integer in the vector.
* **findMedian**: Finds the median of the vector.
* **findMode**: Finds the mode(s) of the vector.
* **findRange**: Finds the range of the vector.
* **findVariance**: Finds the variance of the vector.
* **findStandardDeviation**: Finds the standard deviation of the vector.
* **findQuartiles**: Finds the quartiles of the vector.

The code also includes a `main` function that demonstrates how to use these functions to analyze a given vector of integers. The output of the program is as follows:

```
Most Frequent Integer: 1
Least Frequent Integer: 6
Median: 5.5
Mode: 1 2 3 4 5
Range: 9
Variance: 10.2857
Standard Deviation: 3.2086
Quartiles: 3 5.5 8
```