// Student: Vinicius de Moraes Rego Cousseau (vd2299)
// cs3101-2 - Programming Languages: Scala
// Homework 3
// Part 2

/** Abstract base class for Nodes and Leafs */
abstract class Tree[T] { 
 
    val content : T

    /** A higher-order generalization for tree operations.
      *                   
      * This method provides depth first tree traversal as an 
      * abstraction over different tree operations.
      * Tree operations can be implemented by creating first-class functions 
      * proc_node and proc_leaf and passing them to traverse.
      * The abstract method is implemented in Node and Leaf.                    
      */                     
    def traverse[A](proc_node: (A,A,T) => A, proc_leaf: T=>A) : A

    // Part 2(a) 

    def preOrderNode(left: String, right: String, key: T) : String = "(" + key.toString + " " + left + " " + right + ")"  

    def preOrderLeaf(key: T) : String = key.toString

    override def toString = traverse[String](preOrderNode, preOrderLeaf)
   
    // Part 2(b)

    def findNode(left: List[T], right: List[T], key: T) : List[T] = left:::right

    def findLeaf(key: T) : List[T] = key::Nil

    def leafs : List[T] = traverse[List[T]](findNode, findLeaf)

    // Part 2(c)

    def evaluateNode(left: Int, right: Int, key: T) : Int = {
        if (key.equals("+")) {
            left + right
        } else if (key.equals("-")) {
            left - right
        } else {
            left * right
        }
    }

    def evaluateLeaf(key: T) : Int = key.asInstanceOf[java.lang.String].toInt

    def evaluate : Int = traverse[Int](evaluateNode, evaluateLeaf)
}

/** A Tree with exactly two subtrees. */
class Node[T](val content: T, val left: Tree[T], val right: Tree[T]) extends Tree[T]{

    /** The traverse implementation for Node calls proc_node on the 
      * results returned by calling traverse recusively on each 
      * subtree and the content of this node. 
      */
    def traverse[A](proc_node : (A,A,T) =>A, proc_leaf: T=>A) = 
            proc_node(left.traverse(proc_node, proc_leaf), 
                     right.traverse(proc_node, proc_leaf),
                     content)
}
/** Companion object for the Node -- only used to define an apply method */
object Node {
    def apply[T](content: T, left : Tree[T], right: Tree[T]) = 
        new Node(content, left, right)
}

/** A Tree that does not have any further subtrees
  * (i.e. a single leaf node itself).
  */
class Leaf[T](val content: T) extends Tree[T] {

    /** The traverse implementation for Leaf calls proc_laf on the content of
      * the node. proc_leaf usually just converts the content into the correct
      * result type.
      */
    def traverse[A](proc_node : (A,A,T)=>A, proc_leaf: T=>A) =
        proc_leaf(content) 
}
/**  Companion object for the Leaf, only used to define an apply method */
object Leaf {
    def apply[T](content: T) = new Leaf(content) 
}    


/**
 * Main object to test tree operations. 
 */
object Part2{
  
    def main(args : Array[String]) {
        val tree : Tree[String] = 
            Node("-",
                Node("+",
                    Leaf("3"),
                    Node("*",
                        Leaf("5"),
                        Leaf("6"))),
                Leaf("7"))

        println(tree)
        // Should print (- (+ 3 (* 5 6)) 7)
        
        println(tree.leafs)  
        // Should print List(3, 5, 6, 7)

        println(tree.evaluate)
        // Should print 26       
    }
}
