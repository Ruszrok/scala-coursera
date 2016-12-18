println("countChange: example given in instructions")
println(countChange(4, List(4)) == 1)

println("countChange: example given in instructions")
println(countChange(4, List(1, 2)) == 3)

println("countChange: sorted CHF")
println(countChange(300, List(5, 10, 20, 50, 100, 200, 500)) == 1022)

println("countChange: no pennies")
println(countChange(301, List(5, 10, 20, 50, 100, 200, 500)) == 0)

println("countChange: unsorted CHF")
println(countChange(300, List(500, 5, 50, 100, 20, 200, 10)) == 1022)

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
    l.exists(_.toArray.deep == t.toArray.deep)
  }

  //Use fixed part and non fixed part
  def ChangeIterator(money: List[Int], coins: List[Int], combos: List[List[Int]]): List[List[Int]] =
  {
    if(money.isEmpty) return combos
    val combo = getChangeCombination(money.head, coins, List[Int]())
    val fullCombo = (money.tail ++ combo).sortWith(_ > _)

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
      if(!listContains(combos, fullCombo)) {
        ChangeIterator(fullCombo, coins, fullCombo :: combos)
      } else {
        ChangeIterator(fullCombo, coins, combos)
      }
    }
  }

  val allCombinations = ChangeIterator(List(money), coins.sortWith(_>_), List[List[Int]]() )
  if(allCombinations.isEmpty) 0
  else
  {
    allCombinations.length
  }
}