import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object Part1 {

    val fruit_to_color : Map[String , String ] = Map(" banana " ->" yellow ",
                                                 " blueberry " ->" blue ",
                                                 " cherry " ->"red",
                                                 " lemon " ->" yellow ",
                                                 " kiwi " ->" green ")

    def reverse[A,B](map: Map[A,B]): Map[B,List[A]] = {

        val res = new HashMap[B,List[A]]

        for ((key, value) <- map) {

            var done : Boolean = false

            for ((elem, list) <- res) {
                if (elem == value) {
                    res.put(value, list ::: List(key))
                    done = true
                }
            }

            if (done == false) {
                res.put(value, List(key))
            }
        }

        res
    }
    
    def main(args : Array[String]) {
        val t : Map[String, List[String]] = reverse(fruit_to_color)
        println(t)
    }
}