package cards

package suits {
    sealed abstract class CardSuit
    case object Spades extends CardSuit
    case object Clubs extends CardSuit
    case object Diamonds extends CardSuit
    case object Hearts extends CardSuit
}

// define package values with type hierarchy for card values here 
package values {
    sealed abstract class CardValue
    case class NumberValue(value: Int) extends CardValue
    case object Ace extends CardValue
    case object Jack extends CardValue
    case object Queen extends CardValue
    case object King extends CardValue
}

case class PlayingCard(suit: suits.CardSuit, value: values.CardValue)