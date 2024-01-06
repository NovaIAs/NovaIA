```elixir
defmodule Blockchain do
  defstruct [:chain, :pending_transactions]

  def new() do
    %Blockchain{chain: [%Block{index: 0, timestamp: DateTime.utc_now(), transactions: [], proof: 0}], pending_transactions: []}
  end

  def add_block(%Blockchain{chain: chain, pending_transactions: pending_transactions} = blockchain) do
    new_block = %Block{index: length(chain),
                       timestamp: DateTime.utc_now(),
                       transactions: pending_transactions,
                       proof: proof_of_work(last_block(chain), pending_transactions),
                       previous_hash: hash(last_block(chain))}
    %Blockchain{blockchain | chain: chain ++ [new_block], pending_transactions: []}
  end

  def last_block(chain), do: hd(chain)

  def hash(%Block{index: index, timestamp: timestamp, transactions: transactions, proof: proof, previous_hash: previous_hash}) do
    Crypto.hash(:sha256, "#{index}#{timestamp}#{transactions}#{proof}#{previous_hash}")
  end

  def proof_of_work(last_block, pending_transactions) do
    proof = 0
    loop do
      hash = hash(%Block{index: length(last_block.chain) + 1,
                          timestamp: DateTime.utc_now(),
                          transactions: pending_transactions,
                          proof: proof,
                          previous_hash: last_block.hash})
      if String.starts_with?(hash, "0000"), do: proof, else: proof = proof + 1
    end
  end

  def is_valid?(%Blockchain{chain: chain} = blockchain) do
    Enum.reduce(chain, true, fn block, acc ->
      if block.proof == proof_of_work(block.previous_hash, block.transactions),
         do: acc and block.hash == hash(block),
         else: false
    end)
  end
end

defmodule Block do
  defstruct [:index, :timestamp, :transactions, :proof, :previous_hash]
end

defmodule Miner do
  def mine(%Blockchain{pending_transactions: pending_transactions}) do
    blockchain = Blockchain.new()
    Blockchain.add_block(blockchain, pending_transactions)
  end
end

blockchain = Blockchain.new()
pending_transactions = ["Alice sends Bob 10 coins", "Bob sends Charlie 5 coins"]
blockchain = Blockchain.add_block(blockchain, pending_transactions)
pending_transactions = ["Charlie sends Alice 3 coins"]
blockchain = Blockchain.add_block(blockchain, pending_transactions)
IO.inspect blockchain
```

This code implements a simple blockchain in Elixir. A blockchain is a distributed database that maintains a continuously growing list of records, called blocks. Each block contains a cryptographic hash of the previous block, a timestamp, and transaction data. By design, blockchain is inherently resistant to modification of the data. This code implements the core functionality of a blockchain, including adding blocks to the chain, validating the chain, and mining new blocks.

Here's a breakdown of the code:

1. **Blockchain Module**:
   - `defstruct` defines a struct named `Blockchain` with two fields: `chain` and `pending_transactions`.
   - `new()` function creates a new `Blockchain` instance with an empty chain and an empty list of pending transactions.

2. **Adding Blocks**:
   - `add_block()` function takes a `Blockchain` and a list of pending transactions and adds a new block to the chain.
   - It calculates the proof of work for the new block using `proof_of_work()`.
   - It then adds the new block to the chain and returns the updated `Blockchain`.

3. **Last Block**:
   - `last_block()` function returns the last block in the chain.

4. **Hashing**:
   - `hash()` function takes a `Block` and returns its cryptographic hash.

5. **Proof of Work**:
   - `proof_of_work()` function finds a number (proof) such that the hash of a block with that proof starts with a certain number of zeros.
   - This process is computationally intensive and is used to secure the blockchain against tampering.

6. **Blockchain Validation**:
   - `is_valid?()` function checks if a given blockchain is valid by verifying the hashes and proofs of work of all blocks in the chain.

7. **Block Module**:
   - `defstruct` defines a struct named `Block` with five fields: `index`, `timestamp`, `transactions`, `proof`, and `previous_hash`.

8. **Miner Module**:
   - `mine()` function takes a `Blockchain` with pending transactions and mines a new block, adding it to the chain.

9. **Usage**:
   - We create a new blockchain, add some pending transactions, mine new blocks, and print the resulting blockchain.

This code provides a basic implementation of a blockchain in Elixir, demonstrating the core concepts and functionality of a blockchain system.