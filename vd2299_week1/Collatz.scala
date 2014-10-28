/**
 * Fall 2014 CS3101-2 - Programming Languages: Scala
 * Problem Set 1, Part 1
 * 
 * Using the function collatz, write a function longest collatz, that takes an 
 * integer n as its parameter and return the positive integer m, m <= n for 
 * which collatz needs the largest number of steps. Add your function to the 
 * file Collatz.scala.
 *
 * Answer the following questions as comments at the end of Collatz.scala:
 *
 *   - Which number m, m <= 1000, produces the longest Collatz sequence? How 
 *     many steps are in this sequence?
 *   - For very large n (e.g. n = 1, 000, 000) the naive implementation of 
 *     longest collatz becomes very slow. Explain why. Describe (in words) 
 *     how you could improve the function to terminate faster (you do not
 *     have to implement a better solution at this point. The naive one
 *     is fine).
 */

def collatz_rec(n: Int, counter: Int) : Int = 
    if (n == 1) 
        counter 
    else if (n % 2 == 0) 
        collatz_rec(n / 2, counter + 1)
    else 
        collatz_rec(n * 3 + 1, counter + 1)

def collatz(n:Int) : Int = collatz_rec(n, 0)

// Your code here
def longest_collatz(n : Int) : Int = {
    var max_steps = 0
    var m = 1
    var max = 1
    while (m <= n){
        var curr_steps = collatz(m)
        if (curr_steps > max_steps) {
            max_steps = curr_steps
            max = m
        }
        m += 1
    }
    // Print the number of steps of the longest sequence.
    // Used only to get an answer for one of the questions. 
    //println(max_steps)
    max
}

println(longest_collatz(1000000))

/* 
 * Answers to questions here
 * 
 * The number m,m <= 1000, which produces the longest Collatz sequence is 871.
 * There are 178 steps in this sequence.
 *
 * The naive implementation gets very slow because the collatz function
 * is being called several times and for ever increasing numbers . One way to 
 * improve the longest_collatz function woulde be to use a memoized version 
 * of the collatz function.
 */