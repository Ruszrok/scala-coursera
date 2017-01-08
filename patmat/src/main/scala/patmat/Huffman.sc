import patmat.Huffman

val res = Huffman.createCodeTree("aabcdd".toList)
val res1 = Huffman.encode(res)("abc".toList)
val res4 = Huffman.quickEncode(res)("abc".toList)
val res2 = Huffman.decode(res, res1)