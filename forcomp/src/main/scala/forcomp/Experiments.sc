import forcomp.Anagrams._

var occ = sentenceOccurrences(List("aaa"))
def combine(occ: Occurrences):List[Occurrences] = occ match {
  case List() => List(Nil)
  case (c, n) :: tail =>
    val tailCombos = combine(tail)
    tailCombos ::: (for{
      j <- tailCombos
      i <- 1 to n
    } yield (c, i) :: j)
}

combine(occ)