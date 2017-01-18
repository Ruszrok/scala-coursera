import forcomp.Anagrams._

var occ = sentenceOccurrences(List("aaabbb"))

val occ1 = sentenceOccurrences(List("aabb"))

def subtract(a1: Occurrences, a2: Occurrences): Occurrences = {
  def minus(x:(Char, Int), y: (Char,Int)) : (Char, Int) = {
    if (x._1 == y._1)
        (x._1, x._2-y._2)
    else
      x
  }
  if (a2.isEmpty)
    a1
  else
    subtract(a1.map(minus(_, a2.head)), a2.tail)
}

subtract(occ, occ1)