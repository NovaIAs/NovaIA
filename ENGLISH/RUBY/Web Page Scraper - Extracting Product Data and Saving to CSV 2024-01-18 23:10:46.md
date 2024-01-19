```ruby
# Load the required libraries
require 'nokogiri'
require 'open-uri'
require 'csv'

# Define the URL of the web page to be scraped
url = 'https://www.example.com/products'

# Open the web page and parse its HTML content
html_doc = Nokogiri::HTML(open(url))

# Create an empty array to store the product data
products = []

# Iterate over the HTML elements representing each product
html_doc.css('.product').each do |product|
  # Extract the product's name
  name = product.css('h2.product-name').text.strip

  # Extract the product's price
  price = product.css('.product-price').text.strip

  # Extract the product's description
  description = product.css('.product-description').text.strip

  # Create a hash to store the product data
  product_data = {
    name: name,
    price: price,
    description: description
  }

  # Add the product data to the array
  products << product_data
end

# Open a CSV file for writing
CSV.open('products.csv', 'w') do |csv|
  # Write the headers to the CSV file
  csv << ['Name', 'Price', 'Description']

  # Iterate over the product data and write it to the CSV file
  products.each do |product|
    csv << [product[:name], product[:price], product[:description]]
  end
end

# Print a message to indicate that the scraping is complete
puts "Scraping complete. Data saved to products.csv"
```

Explanation:

1. The code starts by loading the required libraries, including `nokogiri`, `open-uri`, and `csv`.

2. The URL of the web page to be scraped is defined.

3. The web page is opened and its HTML content is parsed using the `Nokogiri` library.

4. An empty array called `products` is created to store the product data.

5. The code then iterates over the HTML elements representing each product on the web page.

6. For each product, the code extracts the product's name, price, and description using the `css` method of the `Nokogiri` library.

7. The extracted data is stored in a hash called `product_data`.

8. The `product_data` hash is then added to the `products` array.

9. A CSV file named `products.csv` is opened for writing.

10. The headers are written to the CSV file using the `<<` method.

11. The product data is then iterated over and written to the CSV file using the `<<` method.

12. A message is printed to the console to indicate that the scraping is complete and the data has been saved to the CSV file.