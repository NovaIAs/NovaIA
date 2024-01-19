```java
import java.util.*;
import java.io.*;
import java.math.*;
import java.util.regex.*;

public class Main {
    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(System.out));
        
        // Read input
        int n = Integer.parseInt(br.readLine());
        int[] a = new int[n];
        for (int i = 0; i < n; i++) {
            a[i] = Integer.parseInt(br.readLine());
        }
        
        // Calculate the frequency of each element in the array
        Map<Integer, Integer> frequencyMap = new HashMap<>();
        for (int element : a) {
            frequencyMap.put(element, frequencyMap.getOrDefault(element, 0) + 1);
        }
        
        // Find the mode of the array (the element that occurs most frequently)
        int maxFrequency = 0;
        int mode = -1;
        for (Map.Entry<Integer, Integer> entry : frequencyMap.entrySet()) {
            if (entry.getValue() > maxFrequency) {
                maxFrequency = entry.getValue();
                mode = entry.getKey();
            }
        }
        
        // Calculate the range of the array (the difference between the largest and smallest elements)
        int minValue = Integer.MAX_VALUE;
        int maxValue = Integer.MIN_VALUE;
        for (int element : a) {
            minValue = Math.min(minValue, element);
            maxValue = Math.max(maxValue, element);
        }
        int range = maxValue - minValue;
        
        // Calculate the mean of the array (the average of the elements)
        double mean = 0;
        for (int element : a) {
            mean += element;
        }
        mean /= n;
        
        // Calculate the median of the array (the middle element when sorted)
        Arrays.sort(a);
        double median;
        if (n % 2 == 0) {
            median = (a[n / 2] + a[n / 2 - 1]) / 2.0;
        } else {
            median = a[n / 2];
        }
        
        // Calculate the standard deviation of the array (a measure of how spread out the data is)
        double standardDeviation = 0;
        for (int element : a) {
            standardDeviation += Math.pow(element - mean, 2);
        }
        standardDeviation = Math.sqrt(standardDeviation / (n - 1));
        
        // Write the output
        bw.write("Mode: " + mode);
        bw.newLine();
        bw.write("Range: " + range);
        bw.newLine();
        bw.write("Mean: " + mean);
        bw.newLine();
        bw.write("Median: " + median);
        bw.newLine();
        bw.write("Standard Deviation: " + standardDeviation);
        bw.newLine();
        
        bw.close();
        br.close();
    }
}
```

This code takes an array of integers as input and calculates various statistical measures of the array, including the mode, range, mean, median, and standard deviation. It uses a HashMap to store the frequency of each element in the array, and then uses this information to calculate the mode. It also uses the Arrays.sort() method to sort the array in order to calculate the median. The code then uses a loop to calculate the mean and standard deviation. Finally, it writes the results to the console.