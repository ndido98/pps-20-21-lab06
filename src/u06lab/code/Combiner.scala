package u06lab.code

/**
 * 1) Implement trait Functions with an object FunctionsImpl such that the code
 * in TryFunctions works correctly.
 */

trait Functions {
    def sum(a: List[Double]): Double

    def concat(a: Seq[String]): String

    def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

object FunctionsImpl extends Functions {

    override def sum(a: List[Double]): Double = a.sum

    override def concat(a: Seq[String]): String = a.foldLeft("")((acc, elem) => acc + elem)

    override def max(a: List[Int]): Int = a.foldLeft(Int.MinValue)((res, elem) => if (elem > res) elem else res)
}


/**
 * 2) To apply DRY principle at the best,
 * note the three methods in Functions do something similar.
 * Use the following approach:
 * - find three implementations of Combiner that tell (for sum,concat and max) how
 * to combine two elements, and what to return when the input list is empty
 * - implement in FunctionsImpl a single method combiner that, other than
 * the collection of A, takes a Combiner as input
 * - implement the three methods by simply calling combiner
 *
 * When all works, note we completely avoided duplications..
 */

trait Combiner[A] {
    def unit: A

    def combine(a: A, b: A): A
}