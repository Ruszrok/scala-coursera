import forcomp.Anagrams

val str = "aaabbcccdd"
val arr = str.groupBy(_.toChar).mapValues(_.size).toList

val dictionary = List[String]("test", "stet", "aq")
val words = dictionary.groupBy(Anagrams.wordOccurrences).toList
//  .map(w =>(Anagrams.wordOccurrences(w), w))
//  .groupBy(p => p._1)
Anagrams.wordOccurrences("asad")
Anagrams.sentenceOccurrences(List("aaa", "b", "cc"))