// A comment!
/* Another comment */
/** A documentation comment */
object StringUtilV1 {
     def joiner(strings: String*): String = strings.mkString("-")
     def joiner(strings: List[String]): String = joiner(strings: _*)
}


object MyModule {
    
    def abs(n: Int): Int =
    if (n < 0) -n
        else n
        
    def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int =
            if (n <= 0) acc
                else go(n-1, n*acc)
        go(n, 1)
    }
        
    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d"
        msg.format(x, abs(x))
    }
    
    private def formatFactorial(n: Int) = {
        val msg = "The factorial of %d is %d."
        msg.format(n, factorial(n))
    }
    
    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d."
        msg.format(name, n, f(n))
    }
    
    def findFirst(ss: Array[String], key: String): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= ss.length) -1
            else if (ss(n) == key) n
            else loop(n + 1)

        loop(0)
    }
    
    def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int =
            if (n >= as.length) -1
            else if (p(as(n))) n
            else loop(n + 1)

        loop(0)
    }
    
    sealed trait List[+A]
    case object Nil extends List[Nothing]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]

    object List {
        def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }
    
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
        
    def sum2(ns: List[Int]) =
        foldRight(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
        foldRight(ns, 1.0)(_ * _)
        
    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
    
    
    def Try[A](a: => A): Option[A] =
        try Some(a)
        catch { case e: Exception => None }
    
    
    def main(args: Array[String]): Unit =
        println(formatAbs(-42))
        println(formatFactorial(4))
        println(formatResult("factorial", 7, factorial))
        println(findFirst(Array("You", "Me"), "Then"))
        println(findFirst(Array(7, 9, 13), (x: Int) => x == 9))
        val absO: Option[Double] => Option[Double] = lift(math.abs)
        println(StringUtilV1.joiner("Programming", "Scala"))
        
}