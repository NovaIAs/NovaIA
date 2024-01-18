```elixir
defmodule Encyclopedia do
  def start do
    IO.puts "Welcome to the Encyclopedia of Life!"
  end

  def get_article(topic) do
    case File.read("encyclopedia/#{topic}.txt") do
      {:ok, content} -> content
      {:error, _} -> "Article not found."
    end
  end

  def list_topics do
    File.ls!("encyclopedia")
    |> Enum.map(fn topic -> String.replace(topic, ".txt", "") end)
  end

  def search(query) do
    Enum.filter(list_topics(), fn topic -> String.contains?(topic, query) end)
  end

  def contribute(topic, content) do
    File.write("encyclopedia/#{topic}.txt", content)
    IO.puts "Article submitted for review."
  end

  def review_articles do
    Enum.each(File.ls!("encyclopedia"), fn topic ->
      content = get_article(topic)
      IO.puts "#{topic}: #{content}"
    end)
  end
end

start()

loop do
  IO.puts "What would you like to do?"
  IO.puts "1. Get an article"
  IO.puts "2. List all articles"
  IO.puts "3. Search for an article"
  IO.puts "4. Contribute an article"
  IO.puts "5. Review articles"
  IO.puts "6. Quit"
  choice = IO.gets("") |> String.trim()

  case choice do
    "1" ->
      IO.puts "Enter the topic of the article:"
      topic = IO.gets("") |> String.trim()
      IO.puts get_article(topic)
    "2" ->
      IO.puts "List of all articles:"
      Enum.each(list_topics(), fn topic -> IO.puts topic end)
    "3" ->
      IO.puts "Enter the query to search for:"
      query = IO.gets("") |> String.trim()
      IO.puts "Search results:"
      Enum.each(search(query), fn topic -> IO.puts topic end)
    "4" ->
      IO.puts "Enter the topic of the article:"
      topic = IO.gets("") |> String.trim()
      IO.puts "Enter the content of the article:"
      content = IO.gets("") |> String.trim()
      contribute(topic, content)
    "5" ->
      IO.puts "Reviewing articles:"
      review_articles()
    "6" ->
      IO.puts "Goodbye!"
      exit(0)
    _ ->
      IO.puts "Invalid choice. Please try again."
  end
end
```

This code implements an interactive encyclopedia application in Elixir. The application allows users to:

* Get an article on a specific topic
* List all available articles
* Search for articles containing a specific query
* Contribute an article for review
* Review submitted articles

The application uses Elixir's File module to read and write articles to the `encyclopedia` directory. The `start()` function initializes the application and prints a welcome message. The `loop` function presents a menu of options to the user and prompts for input. Depending on the user's choice, the code performs the appropriate action, such as getting an article, searching for articles, or contributing an article. The `review_articles()` function prints the content of all submitted articles, allowing the user to review them.

Overall, this code demonstrates the use of Elixir's modules, functions, and control structures to create an interactive application with user-friendly options.