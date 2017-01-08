import patmat.Huffman

Huffman.times("aabb".toList)
Huffman.times("babbc".toList)
val freq = Huffman.times("bdddabbckkkkkkk".toList)
Huffman.makeOrderedLeafList(freq)

val decodedSecret = Huffman.decodedSecret

val res = Huffman.createCodeTree("aabcdd".toList)
val res1 = Huffman.encode(res)("abc".toList)
val res2 = Huffman.decode(res, res1)