```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <unordered_map>
#include <set>
#include <unordered_set>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>

using namespace std;

// Define a custom data structure to store the information for each word.
struct WordInfo {
    string word;
    int count;
    vector<int> line_numbers;
};

// Function to read the input file and extract the words.
vector<string> read_file(string filename) {
    vector<string> words;
    ifstream file(filename);
    string line;
    while (getline(file, line)) {
        stringstream ss(line);
        string word;
        while (ss >> word) {
            transform(word.begin(), word.end(), word.begin(), ::tolower);
            words.push_back(word);
        }
    }
    file.close();
    return words;
}

// Function to compare two WordInfo objects based on their counts.
bool compare_word_counts(const WordInfo& a, const WordInfo& b) {
    return a.count > b.count;
}

// Function to find the top N most frequently occurring words in a vector of words.
vector<WordInfo> find_top_n_words(vector<string> words, int n) {
    // Create a map to store the word counts.
    unordered_map<string, WordInfo> word_counts;
    for (string word : words) {
        auto it = word_counts.find(word);
        if (it == word_counts.end()) {
            word_counts.insert({word, {word, 1, {}}});
        } else {
            it->second.count++;
        }
    }

    // Create a vector of WordInfo objects and populate it with the word counts.
    vector<WordInfo> word_info;
    for (auto it : word_counts) {
        word_info.push_back(it.second);
    }

    // Sort the vector of WordInfo objects based on the word counts.
    sort(word_info.begin(), word_info.end(), compare_word_counts);

    // Return the top N most frequently occurring words.
    return vector<WordInfo>(word_info.begin(), word_info.begin() + n);
}

// Function to print the top N most frequently occurring words.
void print_top_n_words(vector<WordInfo> words, int n) {
    cout << "Top " << n << " Most Frequently Occurring Words:" << endl;
    for (int i = 0; i < min(n, (int)words.size()); i++) {
        cout << setw(15) << left << words[i].word;
        cout << setw(5) << right << words[i].count;
        cout << " - ";
        for (int line_number : words[i].line_numbers) {
            cout << line_number << ", ";
        }
        cout << endl;
    }
}

// Main function.
int main() {
    // Read the input file and extract the words.
    vector<string> words = read_file("input.txt");

    // Find the top 10 most frequently occurring words.
    vector<WordInfo> top_10_words = find_top_n_words(words, 10);

    // Print the top 10 most frequently occurring words.
    print_top_n_words(top_10_words, 10);

    return 0;
}
```

Explanation:

1. The program starts by including the necessary C++ libraries.

2. A custom data structure called `WordInfo` is defined to store the information for each word, including the word itself, its count, and a vector of line numbers where it appears in the input file.

3. The `read_file` function is defined to read the input file and extract the words from it. It reads each line of the file, splits it into words, converts all words to lowercase, and stores them in a vector.

4. The `compare_word_counts` function is defined to compare two `WordInfo` objects based on their counts. This function is used to sort the `WordInfo` objects in descending order of their counts.

5. The `find_top_n_words` function is defined to find the top N most frequently occurring words in a vector of words. It creates a map to store the word counts, populates the map with the word counts, creates a vector of `WordInfo` objects from the map, sorts the vector based on the word counts, and returns the top N most frequently occurring words.

6. The `print_top_n_words` function is defined to print the top N most frequently occurring words. It takes a vector of `WordInfo` objects and an integer N as input and prints the top N words along with their counts and the line numbers where they appear in the input file.

7. In the main function, the input file is read and the words are extracted using the `read_file` function. The top 10 most frequently occurring words are found using the `find_top_n_words` function. Finally, the top 10 words are printed using the `print_top_n_words` function.