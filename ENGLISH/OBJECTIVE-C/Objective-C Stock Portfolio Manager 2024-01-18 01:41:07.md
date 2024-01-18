# Objective-C Code to Implement a Stock Portfolio Manager

```objective-c
#import <Foundation/Foundation.h>

// Define the Stock class
@interface Stock : NSObject
{
    NSString *symbol;
    double price;
    int quantity;
}

// Initialize the stock object
- (id)initWithSymbol:(NSString *)symbol price:(double)price quantity:(int)quantity;

// Get the stock symbol
- (NSString *)symbol;

// Get the stock price
- (double)price;

// Get the stock quantity
- (int)quantity;

// Calculate the total value of the stock
- (double)totalValue;
@end

@implementation Stock

- (id)initWithSymbol:(NSString *)symbol price:(double)price quantity:(int)quantity
{
    self = [super init];
    if (self) {
        self.symbol = symbol;
        self.price = price;
        self.quantity = quantity;
    }
    return self;
}

- (NSString *)symbol
{
    return symbol;
}

- (double)price
{
    return price;
}

- (int)quantity
{
    return quantity;
}

- (double)totalValue
{
    return price * quantity;
}

@end

// Define the StockPortfolio class
@interface StockPortfolio : NSObject
{
    NSMutableArray *stocks;
}

// Initialize the stock portfolio
- (id)init;

// Add a stock to the portfolio
- (void)addStock:(Stock *)stock;

// Remove a stock from the portfolio
- (void)removeStock:(Stock *)stock;

// Get the total value of the portfolio
- (double)totalValue;
@end

@implementation StockPortfolio

- (id)init
{
    self = [super init];
    if (self) {
        stocks = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addStock:(Stock *)stock
{
    [stocks addObject:stock];
}

- (void)removeStock:(Stock *)stock
{
    [stocks removeObject:stock];
}

- (double)totalValue
{
    double total = 0.0;
    for (Stock *stock in stocks) {
        total += [stock totalValue];
    }
    return total;
}

@end

// Create a stock portfolio
StockPortfolio *portfolio = [[StockPortfolio alloc] init];

// Add some stocks to the portfolio
[portfolio addStock:[[Stock alloc] initWithSymbol:@"AAPL" price:100.0 quantity:100]];
[portfolio addStock:[[Stock alloc] initWithSymbol:@"GOOG" price:150.0 quantity:50]];
[portfolio addStock:[[Stock alloc] initWithSymbol:@"MSFT" price:200.0 quantity:25]];

// Get the total value of the portfolio
double totalValue = [portfolio totalValue];

// Print the total value of the portfolio
NSLog(@"Total value of the portfolio: %.2f", totalValue);
```

**Explanation:**

This Objective-C code implements a simple stock portfolio manager. It defines two classes: `Stock` and `StockPortfolio`. The `Stock` class represents a single stock, and the `StockPortfolio` class represents a collection of stocks.

The `Stock` class has three instance variables: `symbol`, `price`, and `quantity`. The `symbol` variable stores the stock symbol, the `price` variable stores the stock price, and the `quantity` variable stores the number of shares of the stock that the user owns.

The `StockPortfolio` class has one instance variable: `stocks`. The `stocks` variable is an array of `Stock` objects.

The `Stock` class has three methods: `initWithSymbol:price:quantity:`, `symbol`, `price`, and `quantity`. The `initWithSymbol:price:quantity:` method is the initializer method for the `Stock` class. It takes three arguments: the stock symbol, the stock price, and the number of shares of the stock that the user owns. The `symbol`, `price`, and `quantity` methods simply return the values of the corresponding instance variables.

The `StockPortfolio` class has four methods: `init`, `addStock:`, `removeStock:`, and `totalValue`. The `init` method is the initializer method for the `StockPortfolio` class. It simply creates an empty array to store the stocks. The `addStock:` method adds a stock to the portfolio. The `removeStock:` method removes a stock from the portfolio. The `totalValue` method calculates the total value of the portfolio by summing the total value of each stock in the portfolio.

The main function of the program creates a stock portfolio, adds some stocks to the portfolio, and then prints the total value of the portfolio.