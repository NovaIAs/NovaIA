```groovy
class AdvancedGroovyScript {

    // Define a custom type alias for a map of strings to integers.
    typedef StringIntegerMap = Map<String, Integer>

    // Define a custom exception for division by zero.
    class DivisionByZeroException extends RuntimeException {

        DivisionByZeroException(String message) {
            super(message)
        }
    }

    // Define a method to calculate the factorial of a number.
    static int factorial(int number) {
        if (number < 0) {
            throw new IllegalArgumentException("Factorial is not defined for negative numbers.")
        }
        if (number == 0) {
            return 1
        }
        return number * factorial(number - 1)
    }

    // Define a method to calculate the Fibonacci sequence up to a specified number.
    static List<Integer> fibonacci(int number) {
        if (number < 0) {
            throw new IllegalArgumentException("Fibonacci sequence is not defined for negative numbers.")
        }
        if (number == 0) {
            return []
        }
        if (number == 1) {
            return [0]
        }
        def fibSequence = [0, 1]
        while (fibSequence[-1] < number) {
            def nextNumber = fibSequence[-2] + fibSequence[-1]
            fibSequence << nextNumber
        }
        return fibSequence
    }

    // Define a method to check if a string is a palindrome.
    static boolean isPalindrome(String str) {
        def normalized = str.toLowerCase().replaceAll(/\W/, '')
        return normalized == normalized.reverse()
    }

    // Define a method to group a list of objects by a specified property.
    static <T> Map<Object, List<T>> groupBy(List<T> list, String property) {
        def groups = list.groupBy { it."$property" }
        groups.collectEntries { k, v -> [(k): v.toList()] }
    }

    // Define a method to sort a list of objects by a specified property.
    static <T> List<T> sortBy(List<T> list, String property, boolean ascending = true) {
        def comparator = { a, b ->
            if (ascending) {
                a."$property".compareTo(b."$property")
            } else {
                b."$property".compareTo(a."$property")
            }
        }
        list.sort(comparator)
    }

    // Define a method to find the maximum value in a list of objects by a specified property.
    static <T> T maxBy(List<T> list, String property) {
        def maxObject = null
        def maxValue = null
        for (obj in list) {
            def value = obj."$property"
            if (maxValue == null || value > maxValue) {
                maxObject = obj
                maxValue = value
            }
        }
        return maxObject
    }

    // Define a method to find the minimum value in a list of objects by a specified property.
    static <T> T minBy(List<T> list, String property) {
        def minObject = null
        def minValue = null
        for (obj in list) {
            def value = obj."$property"
            if (minValue == null || value < minValue) {
                minObject = obj
                minValue = value
            }
        }
        return minObject
    }

    // Define a method to calculate the moving average of a list of numbers.
    static List<Double> movingAverage(List<Double> numbers, int windowSize) {
        if (windowSize <= 0) {
            throw new IllegalArgumentException("Window size must be greater than 0.")
        }
        if (windowSize > numbers.size()) {
            throw new IllegalArgumentException("Window size must be less than or equal to the number of data points.")
        }
        def averages = []
        for (i in 0..(numbers.size() - windowSize)) {
            def window = numbers[i..(i + windowSize - 1)]
            def average = window.sum() / windowSize
            averages << average
        }
        return averages
    }

    // Define a method to perform linear regression on a set of data points.
    static RegressionResult linearRegression(List<Double> xData, List<Double> yData) {
        if (xData.size() != yData.size()) {
            throw new IllegalArgumentException("X and Y data must have the same number of points.")
        }
        if (xData.size() < 2) {
            throw new IllegalArgumentException("At least 2 data points are required for linear regression.")
        }

        // Calculate the mean of the x and y data.
        def meanX = xData.sum() / xData.size()
        def meanY = yData.sum() / yData.size()

        // Calculate the slope and intercept of the regression line.
        def slope = 0.0
        def intercept = 0.0
        for (i in 0..(xData.size() - 1)) {
            slope += (xData[i] - meanX) * (yData[i] - meanY)
            intercept += (xData[i] - meanX) * (xData[i] - meanX)
        }
        slope /= intercept
        intercept = meanY - slope * meanX

        // Return the regression result.
        return new RegressionResult(slope, intercept)
    }

    // Define a class to represent the result of a linear regression.
    static class RegressionResult {

        double slope
        double intercept

        RegressionResult(double slope, double intercept) {
            this.slope = slope
            this.intercept = intercept
        }

        // Define a method to calculate the predicted y value for a given x value.
        double predict(double x) {
            return slope * x + intercept
        }
    }

    // Define a method to convert a string to a boolean value.
    static boolean toBoolean(String str) {
        if (str == null) {
            return false
        }
        str = str.toLowerCase()
        return str == 'true' || str == '1' || str == 'yes'
    }

    // Define a method to convert a string to an integer value.
    static int toInteger(String str) {
        if (str == null) {
            return 0
        }
        str = str.trim()
        try {
            return Integer.parseInt(str)
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid integer value: $str")
        }
    }

    // Define a method to convert a string to a double value.