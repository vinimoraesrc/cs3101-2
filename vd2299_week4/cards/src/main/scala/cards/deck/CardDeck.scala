package cards.deck

import scala.util.Random
import cards.suits._
import cards.values._
import cards.PlayingCard

/**
* A standard 52-card deck implemented as a mutable stack.
*/
class CardDeck {           
    val suit_list = List(Clubs, Diamonds, Hearts, Spades)
    val all_cards = 
    (for (s <- suit_list) yield {
        val face_cards = PlayingCard(s, Jack) :: 
        PlayingCard(s, Queen) ::
        PlayingCard(s, King) :: 
        PlayingCard(s, Ace) :: Nil 
        val number_cards = for (i <- 2 to 10) yield PlayingCard(s, NumberValue(i))
            number_cards.toList ::: face_cards
        }).flatten
    var cards = Random.shuffle(all_cards)

    private def popNextCard = {
        val next = cards.head
        cards = cards.tail
        next
    }
            
    def nextCard = cards match {
        case Nil => { println("Shuffling"); 
                      cards =  Random.shuffle(all_cards) 
                      popNextCard
                    }
        case list => popNextCard
    }
}

