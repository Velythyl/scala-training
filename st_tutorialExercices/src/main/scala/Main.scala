import scala.annotation.tailrec

//https://www.scala-exercises.org/scala_tutorial

object Main extends App {

    def square(x: Double) = x * x
    def sumOfSquares(x: Double, y: Double) = square(x) + square(y)

    println("Test1: x.to(y) " + 1.to(10))
    println("Test2: func " + square(30))
    println("Test3: func calling func " + sumOfSquares(30, 5))

    val temp = sumOfSquares(30, 30)
    println("Test4: val " + temp)

    def abs(x: Double) = if(x>0) x else -x

    println("Test5: if-else " + abs(-30))
    println("Test6: if-else cont. " + abs(30))

    def sqrtIter(guess: Double, x: Double): Double =
        if (isGoodEnough(guess, x)) guess
        else sqrtIter(improve(guess, x), x)

    def improve(guess: Double, x: Double) =
        (guess + x / guess) / 2

    def isGoodEnough(guess: Double, x: Double) =
        abs(guess * guess - x) < 0.001

    def sqrt(x: Double) = sqrtIter(1.0, x)

    println("Test7: recurs " + sqrt(25))

    println("Test8: Object " + SqrtAndSq.sqrt(25))

    def factorial(n: Int): Int = {
        @tailrec
        def iter(x: Int, result: Int): Int =
            if (x == 0) result
            else iter(x - 1, result * x)

        iter(n, 1)
    }

    println("Test9: tail rec " + factorial(3) + "is 6")
    println("Test10: tail rec cont " + factorial(4) + "is 24")

    sealed trait Symbol
    case class Note(name: String, duration: String, octave: Int) extends Symbol
    case class Rest(duration: String) extends Symbol

    val symbol1: Symbol = Note("C", "Quarter", 3)
    val symbol2: Symbol = Rest("Whole")

    println("Test11: case classes " + symbol1 + " " + symbol2)

    //https://www.scala-lang.org/old/node/107.html
    val symbol3: Symbol = Note("C", "Quarter", 3)
    println("Test12: case class comparison: " + symbol1 + "==?" + symbol3 + " : " + (symbol1 == symbol3) )

    def symbolDuration(symbol: Symbol): String = symbol match {
        case Note(name, duration, octave) => duration
        case Rest(duration) => duration
    }

    println("Test13: " + symbol1 + " duration:" + symbolDuration(symbol1))
    //https://stackoverflow.com/questions/32078526/difference-between-case-class-and-case-object

    def cube(x: Int): Int = x*x*x

    def sum(f: Int => Int, lower: Int, higher: Int): Int = {
        @tailrec
        def loop(index: Int, accumulator: Int): Int = {
            if (index > higher) accumulator
            else loop(index + 1, accumulator + f(index))
        }
        loop(lower, 0)
    }

    def sumInts(a: Int, b: Int) = sum(x => x, a, b) //anonymous func instead of def id(x: Int): Int = x
    def sumCubes(a: Int, b: Int) = sum(cube, a, b)
    def sumFactorials(a: Int, b: Int) = sum(factorial, a, b)

    println("Test14: high order func " + sumInts(1, 10))
    println("Test15: high order func cont. " + sumCubes(1, 10))
    println("Test15: high order func cont. " + sumFactorials(1, 10))

    val fruit: List[String] = List("apples", "oranges", "pears")
    val nums: List[Int] = List(1, 2, 3, 4)
    val diag3: List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
    val empty: List[Nothing] = List()

    println("Test16: lists of simple data " + fruit)

    val test: List[Symbol] = List(symbol1, symbol2, symbol3)
    println("Test17: lists of case classes " + test)

    //https://www.scala-exercises.org/scala_tutorial/standard_library
    //https://stackoverflow.com/questions/28507947/adding-types-in-brackets-in-function-definition
    //listMatch[T] says to scala that this thing takes a list of values having type T. Scala will infer T
    def listMatch[T](list: List[T]): String = list match {
        // Lists of `Int` that starts with `1` and then `2`
        case 1 :: 2 :: xs => "1, 2, ..."
        // Lists of length 1
        case x :: Nil => "length of 1"
        // Same as `x :: Nil`
        case List(x) => "length of 1"
        // The empty list, same as `Nil`
        case List() =>"empty list"
        // A list that contains as only element another list that starts with `2`
        case List(2 :: xs) => "only contains List(2, ...)"

        case "apples" :: xs => "apples, ..."
    }

    println("Test18: nums: " + listMatch(nums) + " empty: " + listMatch(empty) + " fruit: " + listMatch(fruit))

    def insertionSort(xs: List[Int]): List[Int] = xs match {
        case List() => List()
        case y :: ys => insert(y, insertionSort(ys))
    }

    def cond(x: Int, y: Int): Boolean = x<=y

    def insert(x: Int, xs: List[Int]): List[Int] =
        xs match {
            case List() => x ::

            case y :: ys =>
                if (cond(x, y)) x :: y :: ys
                else y :: insert(x, ys)
        }
    println("Test19: sort " + insertionSort( (x, y) => x<=y, List(5, 3, 2, 5, 7, 89)))
}
