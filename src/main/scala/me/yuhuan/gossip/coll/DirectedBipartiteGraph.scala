package me.yuhuan.gossip.coll

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait DirectedBipartiteGraph[K1, K2, D12, D21] {

  def keys1: Set[K1]
  def keys2: Set[K2]

  def arcs12: Iterable[(K1, K2, D12)] = for (k1 <- keys1.view; k2 <- outgoingKeysOf1(k1).view; d <- dataAt12(k1, k2)) yield (k1, k2, d)
  def arcs21: Iterable[(K2, K1, D21)] = for (k2 <- keys2.view; k1 <- outgoingKeysOf2(k2).view; d <- dataAt21(k2, k1)) yield (k2, k1, d)

  def dataAt12(k1: K1, k2: K2): Option[D12]
  def dataAt21(k2: K2, k1: K1): Option[D21]

  def outgoingKeysOf1(k: K1): Set[K2]
  def outgoingKeysOf2(k: K2): Set[K1]

}

