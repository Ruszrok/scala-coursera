package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def min(a: Int, b: Int) = if (a < b) a else b

    def layerGenerator(layer: Array[Int]) = {
      val len = min(layer.length, c + 1)
      // All right values redundant
      val arr = new Array[Int](len + 1)
      arr(0) = 1
      for (i <- 1 until len)
        arr(i) = layer(i - 1) + layer(i)
      arr(len) = 0
      arr
    }

    def pascalIter(layer: Array[Int], curRow: Int): Int = {
      if (curRow == r)
        layer(c)
      else {
        val newLayer = layerGenerator(layer)
        pascalIter(newLayer, curRow + 1)
      }
    }

    pascalIter(Array(1, 0), 0)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceChange(c: Char) = c match {
      case ')' => -1
      case '(' => 1
      case _ => 0
    }
    def balanceIter(head: Char, tail: List[Char], balance: Int): Boolean = {
      val newBalance = balance + balanceChange(head)
      if (newBalance < 0 || tail.isEmpty)
        newBalance == 0
      else
        balanceIter(tail.head, tail.tail, newBalance)
    }

    if (chars.isEmpty) true
    else
      balanceIter(chars.head, chars.tail, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def changeVariants(money: Int, coins: List[Int]): Int = {
      val neededCoins = coins.filter(_ <= money)
      if (money == 0) return 1
      if (neededCoins.isEmpty) return 0

      changeVariants(money - neededCoins.head, neededCoins) + changeVariants(money, neededCoins.tail)
    }

    val neededCoins = coins.sortWith(_ > _)
    if (money == 0) 0 else changeVariants(money, neededCoins)
  }
}
