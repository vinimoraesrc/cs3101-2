// Student: Vinicius de Moraes Rego Cousseau (vd2299)
// cs3101-2 - Programming Languages: Scala
// Homework 3
// Part 1

// (a)
def compose(f: Int=>Int, g: Int=>Int): Int=>Int = (x:Int) => f(g(x))

// (b)
def repeat(f: Int=>Int, n: Int): Int=>Int = {
	var x = f
	for (i <- 1 to (n-1)) x = compose(x,f)
	x
}