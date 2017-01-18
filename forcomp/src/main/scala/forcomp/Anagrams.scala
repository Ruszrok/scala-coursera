package forcomp


object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = w.groupBy(_.toLower).mapValues(_.size).toList.sortWith(_._1 < _._1)

  def sentenceOccurrences(s: Sentence): Occurrences = s.flatMap(wordOccurrences)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(Anagrams.wordOccurrences) withDefaultValue Nil

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match{
    case List() => List(Nil)
    case (c,n) :: tail =>
      val tailCombinations = combinations(tail)
      tailCombinations ::: (for{
          combination <- tailCombinations
          j <- 1 to n
        } yield (c,j) :: combination)
  }

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
      subtract(a1.map(minus(_, a2.head)).filter(_._2 > 0), a2.tail)
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def subSentence(occ: Occurrences): List[Sentence] = {
      if (occ.isEmpty) List(List())
      else
        for {
          x <- combinations(occ)
          y <- dictionaryByOccurrences(x)
          z <- subSentence(subtract(occ, x))
        } yield y :: z
    }

    subSentence(sentenceOccurrences(sentence))
  }
}
