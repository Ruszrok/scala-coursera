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
      def min(a: Int, b: Int) = if (a<b) a else b

      def layerGenerator(layer: Array[Int]) = {
        val len = min(layer.length, c + 1)// All right values redundant
        val arr = new Array[Int](len+1)
        arr(0) = 1
        for (i <- 1 until len)
          arr(i) = layer(i-1)+layer(i)
        arr(len) = 0
        arr
      }

      def pascalIter(layer: Array[Int], curRow: Int) : Int = {
        if (curRow == r)
          layer(c)
        else {
          val newLayer = layerGenerator(layer)
          pascalIter(newLayer, curRow + 1)
        }
      }

      pascalIter(Array(1,0), 0)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(head: Char, tail: List[Char], balance: Int) : Boolean = {
      val newBalance = if(head != ')' && head != '(') balance else if(head == '(') balance + 1 else balance -1
      if(newBalance < 0)
        false
      else
      if(tail.isEmpty)
        newBalance == 0
      else{
        balanceIter(tail.head, tail.tail, newBalance)
      }
    }

    if(chars.isEmpty) true
    else
      balanceIter(chars.head, chars.tail, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def getChangeCombination(money: Int, coins: List[Int], result : List[Int]): List[Int] = {
      if(money == 0) result
      else {
        val availableCoins = coins.filter(_ <= money)
        if (availableCoins.isEmpty) List[Int]()
        else {
          val newMoney = money - availableCoins.head
          if (newMoney >= 0) {
            val result1 = availableCoins.head :: result
            getChangeCombination(newMoney, availableCoins, result1)
          }
          else {
            getChangeCombination(money, availableCoins.tail, result)
          }
        }
      }
    }

    def hash(list: List[Int]) = {
      var hash = 0
      for(i <- list.indices){
        hash += list(i)*2^i
      }
      hash
    }

    def listContains(l : List[List[Int]], t :List[Int]) = {
      l.exists(hash(_) == hash(t))
    }

    def ChangeIterator(money: List[Int], coins: List[Int], combos: List[List[Int]]): List[List[Int]] =
    {
      if(money.isEmpty) return combos
      val combo = getChangeCombination(money.head, coins, List[Int]())
      val fullCombo = money.tail ++ combo

      if(combo.length == 1)
      {
        if(!listContains(combos, fullCombo)) {
          ChangeIterator(fullCombo, coins.filter(_ < combo.head), fullCombo :: combos)
        } else {
          ChangeIterator(fullCombo, coins.filter(_ < combo.head), combos)
        }
      }
      else {
        if (combo.length <= 1) {
          combos
        }
        else
          ChangeIterator(fullCombo, coins, fullCombo :: combos)
      }
    }

    val allCombinations = ChangeIterator(List(money), coins.sortWith(_>_), List[List[Int]]() )
    if(allCombinations.isEmpty) 0
    else
    {
      allCombinations.length
    }
  }
  }
