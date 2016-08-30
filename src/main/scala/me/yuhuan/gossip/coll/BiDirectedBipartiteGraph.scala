package me.yuhuan.gossip.coll

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait BiDirectedBipartiteGraph[K1, K2, D12, D21] extends DirectedBipartiteGraph[K1, K2, D12, D21] {

  def incomingKeysOf1(k: K1): Set[K2]
  def incomingKeysOf2(k: K2): Set[K1]

}