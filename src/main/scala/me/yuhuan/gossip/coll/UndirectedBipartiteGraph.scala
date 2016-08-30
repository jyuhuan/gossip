package me.yuhuan.gossip.coll

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait UndirectedBipartiteGraph[K1, K2, D] extends BiDirectedBipartiteGraph[K1, K2, D, D] {

  def edges: Iterable[(K1, K2, D)] = for (k1 <- keys1; k2 <- adjacentKeys1(k1); d <- dataAt(k1, k2)) yield (k1, k2, d)

  def adjacentKeys1(k: K1): Set[K2]
  def adjacentKeys2(k: K2): Set[K1]


  def dataAt(k1: K1, k2: K2): Option[D]

  def incomingKeysOf1(k: K1): Set[K2] = adjacentKeys1(k)
  def incomingKeysOf2(k: K2): Set[K1] = adjacentKeys2(k)
  def outgoingKeysOf1(k: K1): Set[K2] = adjacentKeys1(k)
  def outgoingKeysOf2(k: K2): Set[K1] = adjacentKeys2(k)

  def dataAt12(k1: K1, k2: K2): Option[D] = dataAt(k1, k2)
  def dataAt21(k2: K2, k1: K1): Option[D] = dataAt(k1, k2)

}
