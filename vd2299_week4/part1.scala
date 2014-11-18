// Student: Vinicius de Moraes Rego Cousseau (vd2299)
// cs3101-2 - Programming Languages: Scala
// Homework 4
// Part 1

/** Part 1(a)
 * 
 * Program 1: 
 * bob (1) -> "Bob" is printed and the argument is incremented locally, then returned.
 * (bob(1), 2) is a tuple containing (2, 2)
 * joe(bob (1) ,2) == joe(2, 2) -> "Joe " is printed, then 4 is printed, then 4 is returned
 * bob(joe(bob (1) ,2)), "Bob" is printed, then 5 is returned
 * ron(bob(joe(bob (1) ,2)) ,3 ,4) -> "Ron " is printed, then 8 is printed, then 12 is printed
 * --------------
 * Final output:
 * Bob
 * Joe 
 * 4
 * Bob
 * Ron
 * 8
 * 12
 *
 * --------------
 *
 * Program 2:
 * When the program calls buggy (10), the function keeps being called infinitely many times
 * and the program prints from 10 to the negative limit of Int. foo is never actually called.
 */ 

// Part 1(b)   
object Part1b {

    def bob(x: =>Int ): Int = { println ("Bob"); x + 1 }

    def joe(x: =>Int ,y: =>Int ): Int = {
        println ("Joe ");
        val a = x; val b = y; println (a+b);
        a+b
    }

    def ron(x: =>Int , y: =>Int , z: =>Int) {
        println ("Ron ")
        println (x + y)
        println (x + y + z)
    }

    def buggy (x: =>Int ): Int = {
        println (x);
        buggy (x - 1)
    }

    def foo(x: =>Int , y: =>Int ): Int = {
        println (x)
        x + 2
    }
    
    ron (bob(joe(bob (1) ,2)) ,3 ,4)

    println (foo (1, buggy (10)))

    // main function used for tests
    /*def main(args : Array[String]) {
        ron (bob(joe(bob (1) ,2)) ,3 ,4)
        println()
        println (foo (1, buggy (10)))
    }*/
    
}

/** Part 1(b)
 * 
 * Program 1: 
 * ron(bob(joe(bob (1) ,2)) ,3 ,4) -> "Ron " is printed. Need to evaluate x in println(x+y)
 * bob(joe(bob (1) ,2)) -> "Bob" is printed. Proceed to evaluate joe(bob (1) ,2)
 * joe(bob (1) ,2) -> "Joe " is printed. Proceed to evaluate bob (1), 2
 * bob (1) -> "Bob" is printed, then 2 is returned. Back to joe(bob (1) ,2)
 * joe(bob (1) ,2) -> 4 is printed and then returned. Back to bob(joe(bob (1) ,2))
 * bob(joe(bob (1) ,2)) -> 5 is returned. Back to ron (bob(joe(bob (1) ,2)) ,3 ,4)
 * ron (bob(joe(bob (1) ,2)) ,3 ,4) -> 8 is printed. Need to evaluate again for (x+y+z).
 * bob(joe(bob (1) ,2)) -> "Bob" is printed. Proceed to evaluate joe(bob (1) ,2)
 * joe(bob (1) ,2) -> "Joe " is printed. Proceed to evaluate bob (1), 2
 * bob (1) -> "Bob" is printed, then 2 is returned. Back to joe(bob (1) ,2)
 * joe(bob (1) ,2) -> 4 is printed and then returned. Back to bob(joe(bob (1) ,2))
 * bob(joe(bob (1) ,2)) -> 5 is returned. Back to ron (bob(joe(bob (1) ,2)) ,3 ,4)
 * ron (bob(joe(bob (1) ,2)) ,3 ,4) -> (x+y+z) can now be evaluated. 12 is printed.
 * --------------
 * Final output:
 * Ron
 * Bob
 * Joe
 * Bob 
 * 4
 * 8
 * Bob
 * Joe
 * Bob
 * 4
 * 12
 *
 * --------------
 *
 * Program 2:
 * foo (1, buggy (10)) -> foo prints its first parameter (1) and then returns it summed to 2
 * 3 is printed
 * buggy(10) is never actually called due to lazy evaluation of parameters.
 --------------
 * Final output:
 * 1
 * 3
 *
 */ 