/* cs3101-2 Programming Languages: Scala
 * Final Exam
 * Vinicius de Moraes Rego Cousseau (vd2299)
 * Part 4
*/

object part4 {

    case class Room(var name: String, var seats: Int)

    case class Date(date: List[Int]) {

        var format_date = (x: Int) => if (x < 10) {"0" + x} else {"" + x}

        val month = format_date(date(0))
        val day = format_date(date(1))
        val year = format_date(date(2))
    }

    abstract class Events(room: Room, start: Date, end: Date, part: Int)

    case class Event(var room: Room, var start: Date, var end: Date, var part: Int) extends Events(room, start, end, part)

    case class Course(var room: Room, var start: Date, var end: Date, var part: Int, var instructor: String) extends Events(room, start, end, part)

    // Sample locations, courses and events below
    val schermerhorn_963 = new Room("Schermerhorn 963", 40)
    val pupin_301 = new Room("Pupin Laboratories 301", 280)
    val pupin_329 = new Room("Pupin Laboratories 329", 105)
    val math_203 = new Room("Mathemathics 203", 110)
    val john_jay = new Room("John Jay Dining Hall", 1000)
    val ferris = new Room("Ferris Auditorium", 500)

    val python = new Course(schermerhorn_963, Date(9::10::2014::Nil), Date(10::15::2014::Nil), 30, "Daniel Bauer")

    val scala = new Course(schermerhorn_963, Date(10::22::2014::Nil), Date(12::3::2014::Nil), 29, "Daniel Bauer")

    val ap = new Course(pupin_301, Date(9::2::2014::Nil), Date(12::4::2014::Nil), 192, "Jae Woo Lee")

    val c2 = new Course(math_203, Date(9::2::2014::Nil), Date(12::4::2014::Nil), 40, "Salim A. Altug")

    val astr = new Course(pupin_329, Date(9::2::2014::Nil), Date(12::4::2014::Nil), 83, "Joe Patterson")

    val lights = new Event(john_jay, Date(12::4::2014::Nil), Date(12::4::2014::Nil), 1001)

    val career_fair = new Event(ferris, Date(10::17::2014::Nil), Date(10::17::2014::Nil), 490)
    // End of sample locations, courses and events

    // Function for item (b)
    def itemb {
        // Manually adding events to a list
        val event_list = List(python, scala, ap, c2, astr, lights, career_fair)

        // Iterate through list printing date and location of each event
        for (event <- event_list) {
            event match {
                case Event(room, start, end, part) => {
                    println(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                }
                case Course(room, start, end, part, instr) => { 
                    println(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                }
                case other => Nil
            }
        }
    }

    // Function for the first bullet point in item (c)
    // Should display all events and courses with instructor's name
    def itemc1 {
        // Manually adding events to a list
        val event_list = List(python, scala, ap, c2, astr, lights, career_fair)

        // Iterate through list printing date and location of each event
        // and also printing the instructor of each course
        for (event <- event_list) {

            event match {
                case Event(room, start, end, part) => {
                    println(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                }
                case Course(room, start, end, part, instr) => { 
                    println(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name + ", by " + instr)
                }
                case other => Nil
            }
        }
    }

    // Function for the second bullet point in item (c)
    // Should display both the python and scala courses
    def itemc2 {
        // Manually adding events to a list
        val event_list = List(python, scala, ap, c2, astr, lights, career_fair)

        // Iterate through list printing date and location of each event
        // considering courses only if they take place at Schermerhorn 963
        for (event <- event_list) {
            event match {
                case Event(room, start, end, part) => {
                    println(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                }
                case Course(room, start, end, part, instr) => { 
                    if (room.name == "Schermerhorn 963") {
                        println(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                    }
                }
                case other => Nil
            }
        }
    }

    // Function for the third bullet point in item (c)
    // Should print the event in John Jay Dining Hall
    def itemc3 {
        // Manually adding events to a list
        val event_list = List(python, scala, ap, c2, astr, lights, career_fair)

        // Iterate through list printing date and location of each event
        // if it's overcrowded
        for (event <- event_list) {
            event match {
                case Event(room, start, end, part) => {
                    if (part > room.seats) {
                        print(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                    }
                }
                case Course(room, start, end, part, instr) => {
                    if (part > room.seats) {
                        print(start.month + "/" + start.day + "/" + start.year + " ~ " + end.month + "/" + end.day + "/" + end.year + " | " + room.name)
                    }
                }
                case other => Nil
            }
        }
    }

    def main(args : Array[String]) {
        println("Item (b): \n")
        itemb
        println("------------------------------------------------------------------")
        println("Item (c): \n")
        println("*")
        itemc1
        println("*")
        itemc2
        println("*")
        itemc3
        println()
    }
}