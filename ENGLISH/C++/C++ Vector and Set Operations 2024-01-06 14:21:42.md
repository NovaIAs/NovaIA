```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

using namespace std;

// Define a custom comparator for sorting pairs of integers by their second element
struct CompareBySecond {
    bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
        return a.second < b.second;
    }
};

// Define a function to count the number of occurrences of each element in a vector
map<int, int> count_occurrences(const vector<int>& v) {
    map<int, int> counts;
    for (int x : v) {
        counts[x]++;
    }
    return counts;
}

// Define a function to find the most frequent element in a vector
int most_frequent_element(const vector<int>& v) {
    map<int, int> counts = count_occurrences(v);
    int most_frequent_element = -1;
    int max_count = -1;
    for (auto it = counts.begin(); it != counts.end(); ++it) {
        if (it->second > max_count) {
            most_frequent_element = it->first;
            max_count = it->second;
        }
    }
    return most_frequent_element;
}

// Define a function to find the least frequent element in a vector
int least_frequent_element(const vector<int>& v) {
    map<int, int> counts = count_occurrences(v);
    int least_frequent_element = -1;
    int min_count = INT_MAX;
    for (auto it = counts.begin(); it != counts.end(); ++it) {
        if (it->second < min_count) {
            least_frequent_element = it->first;
            min_count = it->second;
        }
    }
    return least_frequent_element;
}

// Define a function to find all the unique elements in a vector
set<int> unique_elements(const vector<int>& v) {
    set<int> unique_elements;
    for (int x : v) {
        unique_elements.insert(x);
    }
    return unique_elements;
}

// Define a function to find the intersection of two vectors
vector<int> intersection(const vector<int>& v1, const vector<int>& v2) {
    vector<int> intersection;
    for (int x : v1) {
        if (find(v2.begin(), v2.end(), x) != v2.end()) {
            intersection.push_back(x);
        }
    }
    return intersection;
}

// Define a function to find the union of two vectors
vector<int> union_(const vector<int>& v1, const vector<int>& v2) {
    vector<int> union_;
    for (int x : v1) {
        union_.push_back(x);
    }
    for (int x : v2) {
        if (find(union_.begin(), union_.end(), x) == union_.end()) {
            union_.push_back(x);
        }
    }
    return union_;
}

// Define a function to find the difference of two vectors
vector<int> difference(const vector<int>& v1, const vector<int>& v2) {
    vector<int> difference;
    for (int x : v1) {
        if (find(v2.begin(), v2.end(), x) == v2.end()) {
            difference.push_back(x);
        }
    }
    return difference;
}

// Define a function to find the symmetric difference of two vectors
vector<int> symmetric_difference(const vector<int>& v1, const vector<int>& v2) {
    vector<int> symmetric_difference;
    for (int x : v1) {
        if (find(v2.begin(), v2.end(), x) == v2.end()) {
            symmetric_difference.push_back(x);
        }
    }
    for (int x : v2) {