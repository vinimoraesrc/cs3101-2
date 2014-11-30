import scala.io.StdIn.readLine

/**
* A 2-dimensional aquarium simulation.
*/
class Aquarium (val width: Int, val height: Int){

    // Initialize the aquarium
    val aquarium_elements  = for (i <- 1 to width*height) yield None 
    val aquarium_array = Array[Option[AquariumElement]](aquarium_elements:_*)

    /**
     * Adds an AquariumElement to to aquarium. 
     * Will replace any existing element in the same location.
     */
    def add(element : AquariumElement) {
        aquarium_array(element.locY * width + element.locX) = Some(element)
    }

    /**
     * Ask an AquariumElement to decide on its next move. 
     * Try to move the element in the required direction. 
     * Moving fails if the AquariumElement would leave the aquarium grid. 
     * If the destination of the grid is already occupied the resulting
     * collision is handled by handleCollision. 
     */
    def attemptMove(e : AquariumElement) = e match {
        case e: Moving =>
            if (!e.has_moved) {
                e.has_moved = true 
                val move = e.move
                val newX = e.locX + (if (move=='E) 1 else if (move == 'W) -1 else 0)
                val newY = e.locY + (if (move=='S) 1 else if (move == 'N) -1 else 0)

                // Check that the target of this move is within bounds of the aquarium
                if (newX >= 0 && newY >= 0 && newX < width && newY < height ) {
                    aquarium_array(newY * width + newX) match {
                        case None => { // move to empty cell will always succeed
                            aquarium_array(e.locY * width + e.locX) = None
                            aquarium_array(newY * width + newX) = Some(e) 
                            e.locX = newX 
                            e.locY = newY
                        } 
                        case Some(e2) => {  // handle collision
                            handleCollision(e,e2)
                        }
                    }
                }
            }
        case other => null
    }

    /**
     * Handle the collision resulting from e1 moving into the location of 
     * e2. If e1 can eat e2, e1 replaces e2 in its location. If e2 eats
     * e1, e1 just disappears. If neither AquariumElement can eat the other
     * nothing happens and e1 stays in its original location. This mechanism 
     * will prevent Fish from moving into cells that are occupied by Rocks. 
     */
    def handleCollision(e1 : AquariumElement, e2 : AquariumElement) = e1 match {
        case e1 : Eating => e2 match {
            case e2 : Eating =>
                if (e1.eat(e2)) { // e1 eats e2 
                    aquarium_array(e1.locY * width + e1.locX) = None 
                    aquarium_array(e2.locY * width + e2.locX) = Some(e1)
                    e1.locX = e2.locX 
                    e1.locY = e2.locY                         
                } else if (e2.eat(e1)) { // e2 eats e1
                    aquarium_array(e1.locY * width+ e1.locX) = None 
                }

            case other =>
                if (e1.eat(e2)) { // e1 eats e2 
                    aquarium_array(e1.locY * width + e1.locX) = None 
                    aquarium_array(e2.locY * width + e2.locX) = Some(e1)
                    e1.locX = e2.locX 
                    e1.locY = e2.locY                         
                }
        }

        case other => null
    }

    /**
     * Run an iteration of the simulation by calling attempt move for each
     * AquariumElement in the aquarium
     */
    def update = {
        // Bug fix here: only move elements that have not been moved before
        for (position <- aquarium_array; if (!position.isEmpty); element = position.get) 
            element.has_moved = false 
        for (i <- 0 to aquarium_array.size-1) aquarium_array(i) match {
                case Some(element) => if (!element.has_moved) attemptMove(element)
                case None => Unit
        }
    }

    /**
     * Print the current state of the aquarium to the console.
     */
    def draw {
        for (y <- 0 to height-1) {
            for (x<- 0 to width-1) {
                val element : Option[AquariumElement] = aquarium_array(y * width + x)
                print(element match { 
                           case Some(e) => e.symbol
                           case None =>  "."})
            }
        println
        }
    }

}

abstract class AquariumElement(var locX: Int, var locY: Int) {
    val symbol : Char
    val edible : Boolean 
    val size = 0
    var has_moved : Boolean = false;
}

class Rock(locX: Int,locY: Int) extends AquariumElement(locX, locY) {
    val symbol = '#'
    def eat(other : AquariumElement) : Boolean= false 
    def move = 'Stay 
    val edible = false
}

object Rock{ def apply(locX : Int, locY : Int)  = new Rock(locX, locY) }

abstract class LifeForm(locX: Int,locY: Int) extends AquariumElement(locX, locY) {
    val edible = true
}

class Plant(locX: Int,locY: Int) extends LifeForm(locX, locY) {
    val symbol = '$'
    def move = 'Stay 
    def eat(other : AquariumElement) : Boolean = false 
}
object Plant{ def apply(locX : Int, locY : Int)  = new Plant(locX, locY) }

trait Moving {
    def move = List('N,'S,'E,'W)(scala.util.Random.nextInt(4)) 
}

class Fish(locX: Int, locY: Int, s : Int) extends LifeForm(locX, locY) with Moving {
    override val size = s   
    val symbol = 'f'
}

object Fish{ def apply(locX : Int, locY : Int, s: Int)  = new Fish(locX, locY, s) }

trait Eating extends LifeForm {
    def eat(other : AquariumElement) = other.edible && size > other.size
}

class HungryFish(locX: Int, locY: Int, s: Int) extends Fish(locX, locY,s) with Eating {
    override val symbol = 'F'
}

object HungryFish{ def apply(locX : Int, locY : Int, s: Int)  = new HungryFish(locX, locY, s) }

class Crab(locX: Int, locY: Int, s: Int) extends LifeForm(locX, locY) with Moving with Eating {
    override val symbol = 'C'
}

object Crab{ def apply(locX : Int, locY : Int, s : Int) = new Crab(locX, locY, s) }

object AquariumSimulator {   

    /**
     * Run the aquarium simulation
     */
    def main(args: Array[String]) {
        
        val aquarium = new Aquarium(10,10)

        aquarium.add(Rock(1,1))
        aquarium.add(Rock(1,2))
        aquarium.add(Plant(2,3))
        aquarium.add(Fish(3,3,4))
        aquarium.add(HungryFish(3,4,10))
        
        // Simulate the aquarium 
        var input : String  = ""
        do {
            aquarium.draw
            aquarium.update
            input = readLine
        } while (input != "quit")
    }

}
