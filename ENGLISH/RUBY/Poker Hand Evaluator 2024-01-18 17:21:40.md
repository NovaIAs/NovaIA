```ruby
# Initialize a deck of cards
cards = []
suits = ["Hearts", "Diamonds", "Clubs", "Spades"]
values = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"]

suits.each do |suit|
  values.each do |value|
    cards << "#{value} of #{suit}"
  end
end

# Shuffle the deck
cards.shuffle!

# Deal a hand of 5 cards
hand = cards.sample(5)

# Sort the hand by value
hand.sort_by! do |card|
  value = card.split(" ").first
  case value
  when "Jack" then 11
  when "Queen" then 12
  when "King" then 13
  when "Ace" then 14
  else value.to_i
  end
end

# Print the hand
puts "Your hand:"
hand.each do |card|
  puts " - #{card}"
end

# Check for a royal flush
royal_flush = hand.all? do |card|
  card.include?("Ace") && card.include?("King") && card.include?("Queen") && card.include?("Jack") && card.include?("10")
end

if royal_flush
  puts "You have a royal flush!"
else
  puts "You do not have a royal flush."
end

# Check for a straight flush
straight_flush = hand.all? do |card|
  suit = card.split(" ").last
  value = card.split(" ").first
  case value
  when "Jack" then 11
  when "Queen" then 12
  when "King" then 13
  when "Ace" then 14
  else value.to_i
  end
  if hand.index(card) == 0
    previous_value = value
  else
    previous_value = hand[hand.index(card) - 1].split(" ").first
    case previous_value
    when "Jack" then 11
    when "Queen" then 12
    when "King" then 13
    when "Ace" then 14
    else previous_value.to_i
    end
  end
  value - previous_value == 1
end

if straight_flush
  puts "You have a straight flush!"
else
  puts "You do not have a straight flush."
end

# Check for a four of a kind
four_of_a_kind = hand.group_by { |card| card.split(" ").first }.any? { |_, cards| cards.count == 4 }

if four_of_a_kind
  puts "You have a four of a kind!"
else
  puts "You do not have a four of a kind."
end

# Check for a full house
full_house = hand.group_by { |card| card.split(" ").first }.any? { |_, cards| cards.count == 3 } && hand.group_by { |card| card.split(" ").first }.any? { |_, cards| cards.count == 2 }

if full_house
  puts "You have a full house!"
else
  puts "You do not have a full house."
end

# Check for a flush
flush = hand.all? do |card|
  card.include? hand[0].split(" ").last
end

if flush
  puts "You have a flush!"
else
  puts "You do not have a flush."
end

# Check for a straight
straight = hand.all? do |card|
  suit = card.split(" ").last
  value = card.split(" ").first
  case value
  when "Jack" then 11
  when "Queen" then 12
  when "King" then 13
  when "Ace" then 14
  else value.to_i
  end
  if hand.index(card) == 0
    previous_value = value
  else
    previous_value = hand[hand.index(card) - 1].split(" ").first
    case previous_value
    when "Jack" then 11
    when "Queen" then 12
    when "King" then 13
    when "Ace" then 14
    else previous_value.to_i
    end
  end
  value - previous_value == 1
end

if straight
  puts "You have a straight!"
else
  puts "You do not have a straight."
end

# Check for a three of a kind
three_of_a_kind = hand.group_by { |card| card.split(" ").first }.any? { |_, cards| cards.count == 3 }

if three_of_a_kind
  puts "You have a three of a kind!"
else
  puts "You do not have a three of a kind."
end

#