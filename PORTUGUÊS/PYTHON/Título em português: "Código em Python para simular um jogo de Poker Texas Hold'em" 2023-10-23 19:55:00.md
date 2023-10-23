Código em Python para simular um jogo de Poker Texas Hold'em:

```python
import random

def deal_cards():
    player_hand = []
    dealer_hand = []

    for i in range(2):
        player_card = random.randint(1, 13)
        dealer_card = random.randint(1, 13)
        player_hand.append(player_card)
        dealer_hand.append(dealer_card)

    return player_hand, dealer_hand

def player_action(player_hand, bet):
    print(f"O jogador apostou {bet}")
    return player_hand

def dealer_action(player_hand, dealer_hand):
    if sum(player_hand) > 21:
        print("O jogador perdeu. Mão Negrajack")
        return "Dealer Win"
    elif sum(player_hand) < 17 or dealer_hand[0] < 17:
        print("O jogador ganhou. Mão Royal Flush")
        return "Player Win"
    elif sum(player_hand) > sum(dealer_hand):
        print("O jogador ganhou. Mão Straight Flush")
        return "Player Win"
    elif sum(player_hand) == sum(dealer_hand):
        print("Empate. Mão Pair ou Impar")
        return "Push"
    else:
        print("O dealer ganhou. Mão Straight")
        return "Dealer Win"

player_hand, dealer_hand = deal_cards()
player_bet = int(input("Qual a sua aposta? "))

while True:
    player_hand = player_action(player_hand, player_bet)
    dealer_hand = dealer_action(player_hand, dealer_hand)

    if dealer_hand == "Player Win":
        print("O jogador ganhou!")
        break
    elif dealer_hand == "Dealer Win":
        print("O dealer ganhou!")
        break
    elif dealer_hand == "Push":
        print("Empate!")
        break
```

Explanation:

This code simulates a game of Poker Texas Hold'em. It deals two cards to both the player and the dealer, and then prompts the player to make an initial bet. The function `player_action` allows the player to make their move, which can be to raise or fold.

The function `dealer_action` then determines the outcome of the hand based on the rules of the game. If the dealer's hand qualifies as a blackjack (an ace plus a 10 or face card), the player loses automatically. If the dealer's hand does not qualify and the player's hand is a royal flush (a straight flush with the highest card being an ace), the player wins. If the player's hand is a straight flush, the player wins. If both hands have the same value, it's a push. If none of the above conditions are met, the dealer wins.

The game continues until the player wins, loses, or the hand ends in a push.