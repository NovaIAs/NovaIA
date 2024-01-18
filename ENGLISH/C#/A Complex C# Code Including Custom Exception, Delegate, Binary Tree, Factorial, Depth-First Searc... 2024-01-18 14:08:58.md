```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace ComplexCSharpCode
{
    class Program
    {
        static void Main(string[] args)
        {
            // Define a custom exception type
            public class MyCustomException : Exception
            {
                public MyCustomException(string message) : base(message) { }
            }

            // Define a delegate for a function that takes an integer and returns an integer
            delegate int IntFunction(int n);

            // Define a class that represents a binary tree node
            public class TreeNode
            {
                public int Value { get; set; }
                public TreeNode LeftChild { get; set; }
                public TreeNode RightChild { get; set; }
            }

            // Define a method to calculate the factorial of a number
            static int Factorial(int n)
            {
                if (n == 0)
                {
                    return 1;
                }
                else
                {
                    return n * Factorial(n - 1);
                }
            }

            // Define a method to perform a depth-first search of a binary tree
            static void DepthFirstSearch(TreeNode root)
            {
                if (root == null)
                {
                    return;
                }

                Console.WriteLine(root.Value);
                DepthFirstSearch(root.LeftChild);
                DepthFirstSearch(root.RightChild);
            }

            // Define a method to find the maximum value in a list of integers
            static int MaxValue(List<int> list)
            {
                if (list == null || list.Count == 0)
                {
                    throw new MyCustomException("The list cannot be null or empty.");
                }

                int maxValue = list[0];
                for (int i = 1; i < list.Count; i++)
                {
                    if (list[i] > maxValue)
                    {
                        maxValue = list[i];
                    }
                }

                return maxValue;
            }

            // Define a method to sort a list of strings in ascending order
            static void SortStringsAscending(List<string> list)
            {
                if (list == null)
                {
                    throw new ArgumentNullException("The list cannot be null.");
                }

                list.Sort();
            }

            // Define a method to convert a string to a number
            static int ParseNumber(string str)
            {
                if (str == null)
                {
                    throw new ArgumentNullException("The string cannot be null.");
                }

                int number;
                if (!int.TryParse(str, out number))
                {
                    throw new MyCustomException("The string is not a valid number.");
                }

                return number;
            }

            // Define a method to find the intersection of two sets of numbers
            static HashSet<int> IntersectSets(HashSet<int> set1, HashSet<int> set2)
            {
                if (set1 == null || set2 == null)
                {
                    throw new ArgumentNullException("The sets cannot be null.");
                }

                return set1.Intersect(set2).ToHashSet();
            }

            // Define a method to create a dictionary from a list of key-value pairs
            static Dictionary<string, string> CreateDictionary(List<KeyValuePair<string, string>> keyValuePairs)
            {
                if (keyValuePairs == null)
                {
                    throw new ArgumentNullException("The list of key-value pairs cannot be null.");
                }

                return keyValuePairs.ToDictionary(kvp => kvp.Key, kvp => kvp.Value);
            }

            // Define a method to find the longest common substring between two strings
            static string LongestCommonSubstring(string str1, string str2)
            {
                if (str1 == null || str2 == null)
                {
                    throw new ArgumentNullException("The strings cannot be null.");
                }

                int[,] lcsMatrix = new int[str1.Length + 1, str2.Length + 1];
                int longestSubstringLength = 0;
                int longestSubstringStartIndex = 0;

                for (int i = 1; i <= str1.Length; i++)
                {
                    for (int j = 1; j <= str2.Length; j++)
                    {
                        if (str1[i - 1] == str2[j - 1])
                        {
                            lcsMatrix[i, j] = lcsMatrix[i - 1, j - 1] + 1;
                            if (lcsMatrix[i, j] > longestSubstringLength)
                            {
                                longestSubstringLength = lcsMatrix[i, j];
                                longestSubstringStartIndex = i - longestSubstringLength;
                            }
                        }
                        else
                        {
                            lcsMatrix[i, j] = 0;
                        }
                    }
                }

                return str1.Substring(longestSubstringStartIndex, longestSubstringLength);
            }

            // Define a method to find the shortest path between two nodes in a graph
            static List<int> ShortestPath(Graph graph, int startNode, int endNode)
            {
                if (graph == null || !graph.Nodes.ContainsKey(startNode) || !graph.Nodes.ContainsKey(endNode))
                {
                    throw new ArgumentException("Invalid graph or start/end nodes.");
                }

                Queue<int> queue = new Queue<int>();
                Dictionary<int, int> previousNodes = new Dictionary<int, int>();

                queue.Enqueue(startNode);
                previousNodes[startNode] = -1;

                while (queue.Count > 0)
                {
                    int currentNode = queue.Dequeue();
                    if (currentNode == endNode)
                    {
                        break;
                    }

                    foreach (int neighbor in graph.Nodes[currentNode])
                    {
                        if (!previousNodes.ContainsKey(neighbor))
                        {
                            queue.Enqueue(neighbor);
                            previousNodes[neighbor] = currentNode;
                        }
                    }
                }

                if (!previousNodes.ContainsKey(endNode))
                {
                    return new List<int>(); // No path found
                }

                List<int> shortestPath = new List<int>();
                int currentNode = endNode;
                while (currentNode != -1)
                {
                    shortestPath.Add(currentNode);
                    currentNode = previousNodes[currentNode];
                }

                shortestPath.Reverse();
                return shortestPath;
            }

            // Define a class to represent a graph
            public class Graph
            {
                public Dictionary<int, List<int>> Nodes { get; set; }

                public Graph()
                {
                    Nodes = new Dictionary<int, List<int>>();
                }

                public void AddNode(int node)
                {
                    if (!Nodes.ContainsKey(node))
                    {
                        Nodes[node] = new List<int>();
                    }
                }

                public void AddEdge