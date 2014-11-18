package blackjack 

import scala.io.StdIn.readLine
import cards._
import suits._
import values._
import cards.deck._
// add imports here...



object Blackjack{

    /**
     * Compute the value of a PlayingCard given a current sum of cards. 
     */
    def cardValue(card: PlayingCard, current_sum: Int): Int = {
        val card_value = card.value match {
            case NumberValue(value) => value
            case Ace if (current_sum + 11 <= 21) => 11
            case Ace if (current_sum + 11 > 21) => 1
            case Jack | Queen | King => 10
            case _ => -1 // Undefined value
        }
        card_value
    }

    def main(args: Array[String]) {

        //val card1 = PlayingCard(Hearts, Queen)
        //val card2 = PlayingCard(Diamonds, NumberValue(9))
        //val card3 = PlayingCard(Spades, Ace)

        //println(card1, cardValue(card1,0))
        // Should be 10 
        //println(card2, cardValue(card2,10))
        // Should be 9
        //println(card3, cardValue(card3,19))
        // Should be 1
        //println(card3, cardValue(card3,2))
        // Should be 11

        // comment the cardValue examples and add game code for Part 3 here

        var credits = 100
        val deck = new CardDeck()

        // Infinitely many rounds
        while (true) {
            var points = 0
            var dealer_points = 0
            var hit = true
            var game_over = false

            // Player rounds
            println("PLAYER TURN. Current credits: " + credits)
            while (hit) {
                val curr_card = deck.nextCard
                println(curr_card)

                points += cardValue(curr_card, points)
                println("Current sum: " + points)

                if (points == 21) {
                    println("Player wins!\n")
                    credits += 1
                    hit = false;
                    game_over = true
                } else if (points > 21) {
                    println("Dealer wins!\n")
                    credits -= 1
                    hit = false;
                    game_over = true
                } else {
                    var valid_input = false

                    var r: String = ""

                    // Queries user for action until valid input is received
                    while (!valid_input) {
                        println("Hit or Stand? [H/S]")
                        r = readLine()

                        valid_input = r.toLowerCase() match {
                            case "s" => true
                            case "h" => true
                            case _ => false
                        }
                    }

                    hit = r.toLowerCase() match {
                        case "s" => false
                        case "h" => true
                    }
                }
            }

            // Dealer rounds
            if (!game_over) println("DEALER TURN")
            while (!game_over) {
                val curr_card = deck.nextCard
                println(curr_card)

                dealer_points += cardValue(curr_card, dealer_points)
                println("Dealer sum: " + dealer_points)

                if (dealer_points > 21){
                    println("Player wins!\n")
                    credits += 1
                    game_over = true
                } else if (dealer_points == 21){
                    println("Dealer wins!\n")
                    credits -= 1
                    game_over = true
                } else if (dealer_points >= 17){
                    if (dealer_points > points) {
                        println("Dealer wins!\n")
                        credits -= 1
                        game_over = true
                    } else if (dealer_points < points) {
                        println("Player wins!\n")
                        credits += 1
                        game_over = true
                    } else { // ==
                        println("Round tied!\n")
                        game_over = true
                    }
                }
            }
        }
    }
}

