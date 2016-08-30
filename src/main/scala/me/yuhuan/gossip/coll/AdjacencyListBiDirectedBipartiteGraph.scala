package me.yuhuan.gossip.coll

import me.yuhuan.gossip._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait AdjacencyListBiDirectedBipartiteGraph[K1, K2, D12, D21] extends BiDirectedBipartiteGraph[K1, K2, D12, D21] {

  def outMap12: Map[K1, Set[K2]]
  def outMap21: Map[K2, Set[K1]]

  def inMap12: Map[K1, Set[K2]]
  def inMap21: Map[K2, Set[K1]]

  def incomingKeysOf1(k: K1): Set[K2] = inMap12(k)
  def incomingKeysOf2(k: K2): Set[K1] = inMap21(k)
  def outgoingKeysOf1(k: K1): Set[K2] = outMap12(k)
  def outgoingKeysOf2(k: K2): Set[K1] = outMap21(k)

}

object AdjacencyListBiDirectedBipartiteGraph {

  def apply[K1, K2, D12, D21](_outMap12: Map[K1, Set[K2]], _outMap21: Map[K2, Set[K1]], data12: Map[(K1, K2), D12], data21: Map[(K2, K1), D21]): AdjacencyListBiDirectedBipartiteGraph[K1, K2, D12, D21] = {
    new AdjacencyListBiDirectedBipartiteGraph[K1, K2, D12, D21] {
      val outMap12: Map[K1, Set[K2]] = _outMap12
      val outMap21: Map[K2, Set[K1]] = _outMap21
      val inMap12: Map[K1, Set[K2]] = invert(outMap21)
      val inMap21: Map[K2, Set[K1]] = invert(outMap12)

      val keys1: Set[K1] = _outMap12.keySet
      val keys2: Set[K2] = _outMap21.keySet

      def dataAt12(k1: K1, k2: K2): Option[D12] = data12.get(k1 -> k2)
      def dataAt21(k2: K2, k1: K1): Option[D21] = data21.get(k2 -> k1)
    }
  }

  def apply[K1, K2, D12, D21](outArcMap12: Map[K1, Set[(K2, D12)]], outArcMap21: Map[K2, Set[(K1, D21)]]): AdjacencyListBiDirectedBipartiteGraph[K1, K2, D12, D21] = {
    val outMap12 = outArcMap12.map { case (k1, ps) => k1 -> ps.map(_._1) }
    val outMap21 = outArcMap21.map { case (k2, ps) => k2 -> ps.map(_._1) }
    val data12 = for ((k1, ps) <- outArcMap12; (k2, d) <- ps) yield (k1, k2) -> d
    val data21 = for ((k2, ps) <- outArcMap21; (k1, d) <- ps) yield (k2, k1) -> d
    AdjacencyListBiDirectedBipartiteGraph(outMap12, outMap21, data12, data21)
  }

}

