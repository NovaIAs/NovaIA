```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

// Custom Comparator Function for Sorting
struct CustomComparator {
    bool operator()(const std::pair<int, std::string>& a, const std::pair<int, std::string>& b) {
        // Sort by the second element (string) in ascending order
        return a.second < b.second;
    }
};

// Function to Group Elements by a Key
template <typename T, typename K>
std::map<K, std::vector<T>> groupBy(const std::vector<T>& vec, std::function<K(const T&)> keyExtractor) {
    std::map<K, std::vector<T>> groupedMap;
    for (const auto& element : vec) {
        groupedMap[keyExtractor(element)].push_back(element);
    }
    return groupedMap;
}

int main() {
    // Sample Data
    std::vector<std::pair<int, std::string>> data = {
        {1, "Apple"},
        {2, "Orange"},
        {3, "Banana"},
        {4, "Grape"},
        {5, "Strawberry"},
        {6, "Pineapple"},
        {7, "Watermelon"},
        {8, "Mango"},
        {9, "Blueberry"},
        {10, "Raspberry"}
    };

    // Group the data by the second element (string)
    std::map<std::string, std::vector<std::pair<int, std::string>>> groupedData = groupBy(data, [](const std::pair<int, std::string>& pair) { return pair.second; });

    // Sort the groups by the group key (string)
    std::map<std::string, std::vector<std::pair<int, std::string>>> sortedGroupedData;
    for (const auto& group : groupedData) {
        std::sort(group.second.begin(), group.second.end(), CustomComparator());
        sortedGroupedData[group.first] = group.second;
    }

    // Print the sorted grouped data
    for (const auto& group : sortedGroupedData) {
        std::cout << "Group: " << group.first << std::endl;
        for (const auto& element : group.second) {
            std::cout << "\t" << element.first << ": " << element.second << std::endl;
        }
    }

    return 0;
}
```

**Explanation:**

1. The code defines a custom comparator function `CustomComparator` to compare two pairs of integers and strings based on the string value.

2. It defines a generic function `groupBy` that takes a vector of elements and a key extractor function. The function groups elements by the key extracted using the key extractor function.

3. In the `main` function, sample data is created as a vector of pairs of integers and strings.

4. The `groupBy` function is called to group the sample data by the string value of each pair.

5. The grouped data is then sorted by the group key (string) using a custom comparator.

6. Finally, the sorted grouped data is printed, showing each group and its sorted elements.