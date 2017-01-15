import forcomp.Anagrams

val str = "aaabbcccdd"
val arr = str.groupBy(_.toChar).mapValues(_.size).toList


Anagrams.wordOccurrences("asad")
Anagrams.sentenceOccurrences(List("aaa", "b", "cc"))