/* cs3101-2 Programming Languages: Scala
 * Final Exam
 * Vinicius de Moraes Rego Cousseau (vd2299)
 * Part 3
*/

object part3 {

    def closeEnough(x:Double, y:Double) : Boolean = {
        scala.math.abs(x-y) < 0.00001
    }

    def findFixedPoint[A](fun : A => A, initial_value : A, close_enough : (A, A) => Boolean) : A = {
        val ret = fun (initial_value)
        if (close_enough(initial_value, ret) == true) {
            ret
        } else {
            findFixedPoint(fun, ret, close_enough)
        }
    }

    def main(args : Array[String]) {
        println(findFixedPoint[Double](scala.math.cos, -1.0, closeEnough))
    }
}
