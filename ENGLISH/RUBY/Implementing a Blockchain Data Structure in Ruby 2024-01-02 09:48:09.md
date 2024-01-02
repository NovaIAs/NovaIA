Sure! Please find below a complex and unique Ruby code that implements a blockchain data structure. The code consists of multiple classes and functions, each with its own specific purpose.

```ruby
require 'digest'

# Block class represents a single block in the blockchain
class Block
  attr_reader :timestamp, :data, :previous_hash, :hash

  def initialize(data, previous_hash)
    @timestamp = Time.now
    @data = data
    @previous_hash = previous_hash
    mine_block # Calculate the hash of the block
  end

  def mine_block
    nonce = 0
    loop do
      @hash = calculate_hash(nonce)
      break if @hash.start_with?('0000')
      nonce += 1
    end
  end

  def calculate_hash(nonce)
    Digest::SHA256.hexdigest("#{@timestamp}:#{@data}:#{@previous_hash}:#{nonce}")
  end
end

# Blockchain class manages the blockchain data structure
class Blockchain
  def initialize
    @chain = [create_genesis_block]
  end

  def create_genesis_block
    Block.new('Genesis Block', '0')
  end

  def add_block(data)
    previous_block = @chain.last
    new_block = Block.new(data, previous_block.hash)
    @chain << new_block
  end

  def is_valid?
    (1...@chain.length).each do |i|
      current_block = @chain[i]
      previous_block = @chain[i - 1]

      return false unless current_block.hash == current_block.calculate_hash(0)
      return false unless current_block.previous_hash == previous_block.hash
    end

    true
  end

  def print_chain
    @chain.each do |block|
      puts "Timestamp: #{block.timestamp}"
      puts "Data: #{block.data}"
      puts "Hash: #{block.hash}"
      puts "Previous Hash: #{block.previous_hash}"
      puts '-' * 50
    end
  end
end

# Usage example
blockchain = Blockchain.new
blockchain.add_block('Transaction 1')
blockchain.add_block('Transaction 2')

puts "Blockchain is valid? #{blockchain.is_valid?}"
blockchain.print_chain
```

In this code, we have two main classes: `Block` and `Blockchain`. 

The `Block` class represents a single block in the blockchain. It has the following attributes:
- `timestamp`: The timestamp when the block is created.
- `data`: The data stored in the block.
- `previous_hash`: The hash of the previous block in the chain.
- `hash`: The hash of the current block, calculated using the SHA256 algorithm.

The `Block` class also has two methods:
- `initialize`: Initializes a new block by setting the timestamp, data, and previous hash. It then calls the `mine_block` method to calculate the block's hash.
- `mine_block`: Uses a proof-of-work algorithm to find a nonce value that results in a hash starting with four zeros. This ensures the block is difficult to tamper with.
- `calculate_hash`: Calculates the hash of the block by combining the timestamp, data, previous hash, and nonce.

The `Blockchain` class manages the blockchain data structure. It has the following attributes:
- `chain`: An array that holds all the blocks in the blockchain.

The `Blockchain` class has the following methods:
- `initialize`: Initializes a new blockchain with a genesis block (the first block in the chain).
- `create_genesis_block`: Creates the genesis block with arbitrary data and a previous hash of '0'.
- `add_block`: Adds a new block to the blockchain by specifying the block data and the previous block's hash.
- `is_valid?`: Checks if the entire blockchain is valid by verifying that each block's hash matches its calculated hash and that each block's previous hash matches the previous block's hash.
- `print_chain`: Prints the information of each block in the blockchain, including the timestamp, data, hash, and previous hash.

At the end of the code, there is an example usage section where a blockchain is created, and two blocks are added with different transaction data. The validity of the blockchain is checked, and the entire blockchain is printed to the console.

This code demonstrates a simplified implementation of a blockchain in Ruby and can serve as a starting point for building more complex blockchain applications.