import patmat.Huffman

Huffman.times("aabb".toList)
Huffman.times("babbc".toList)
val freq = Huffman.times("bdddabbckkkkkkk".toList)
Huffman.makeOrderedLeafList(freq)