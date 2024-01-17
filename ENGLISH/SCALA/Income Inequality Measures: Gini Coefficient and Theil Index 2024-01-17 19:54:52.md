```scala
// This code computes the Gini coefficient and the Theil index for a given income distribution.

// Define the income distribution as an array of doubles.
val incomes = Array(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

// Compute the Gini coefficient.

// Sort the income distribution in ascending order.
val sortedIncomes = incomes.sorted

// Compute the cumulative income distribution.
val cumulativeIncomes = sortedIncomes.scanLeft(0.0)((a, b) => a + b)

// Compute the Gini coefficient.
val giniCoefficient = (2 * (incomes.sum - cumulativeIncomes.sum) / (incomes.sum * incomes.length)) - 1

// Compute the Theil index.

// Compute the mean income.
val meanIncome = incomes.sum / incomes.length

// Compute the squared deviations from the mean income.
val squaredDeviations = incomes.map(income => Math.pow(income - meanIncome, 2))

// Compute the Theil index.
val theilIndex = (squaredDeviations.sum / meanIncome) / incomes.length

// Print the Gini coefficient and the Theil index.
println("Gini coefficient:", giniCoefficient)
println("Theil index:", theilIndex)
```

Explanation:

The code first defines the income distribution as an array of doubles. Then, it computes the Gini coefficient and the Theil index for this income distribution.

The Gini coefficient is a measure of statistical dispersion intended to represent the income or wealth distribution of a nation's residents. It is most commonly used as a measure of inequality. The Gini coefficient is defined as the ratio of the cumulative proportion of income received by the poorest half of the population to the cumulative proportion of income received by the richest half of the population. The Gini coefficient can range from 0 to 1, with 0 representing perfect equality and 1 representing perfect inequality.

The Theil index is a measure of income inequality that takes into account the distribution of income across the entire population. The Theil index is defined as the ratio of the sum of the squared deviations from the mean income to the mean income. The Theil index can range from 0 to infinity, with 0 representing perfect equality and infinity representing perfect inequality.

The code first sorts the income distribution in ascending order. Then, it computes the cumulative income distribution by scanning the sorted income distribution from left to right and adding each income to the previous cumulative income. Then, it computes the Gini coefficient using the formula given above.

Next, the code computes the Theil index. First, it computes the mean income by dividing the sum of the incomes by the number of incomes. Then, it computes the squared deviations from the mean income by squaring the difference between each income and the mean income. Finally, it computes the Theil index using the formula given above.

Finally, the code prints the Gini coefficient and the Theil index to the console.