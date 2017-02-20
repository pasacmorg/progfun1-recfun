package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("Balance")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-".toList))
    println(balance("())(".toList))

    println("Change")
    println(countChange(4,List(1,2)))
    println(countChange(300,List(5,10,20,50,100,200,500)))
    println(countChange(301,List(5,10,20,50,100,200,500)))
    println(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      if (chars.indexOf(')') < 0 && chars.indexOf('(') < 0) return false
      def inspect(chars: List[Char], depth: Int): Boolean =
        if (chars.isEmpty) depth == 0
        else if (depth < 0) false
        else if (chars.head == '(') inspect(chars.tail, depth + 1)
        else if (chars.head == ')') inspect(chars.tail, depth - 1)
        else inspect(chars.tail, depth)
      inspect(chars, 0)
    }
  
  /**
   * Exercise 3
    * Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.
    Hint: Think of the degenerate cases. How many ways can you give change for 0 CHF(swiss money)? How many ways can you give change for >0 CHF, if you have no coins?
*/
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 1 || coins.length < 1) 0
    val combs = (1 to coins.size).flatMap(coins.combinations).toList
    def tryCombs(money: Int, combs: List[List[Int]]): Int = {
      def tryComb(money: Int, comb: List[Int]): Int = {
        def trySortedComb(rem: Int, sortedComb: List[Int]): Int = {
          if (rem == 0)  1
          else if (sortedComb.isEmpty)  0
          else if (sortedComb.head > rem) 0
          else trySortedComb(rem % sortedComb.head, sortedComb.tail)
        }
        trySortedComb(money, comb.sorted)
      }
      tryComb(money, combs.head)
    }
    tryCombs(money, combs)
  }
}
