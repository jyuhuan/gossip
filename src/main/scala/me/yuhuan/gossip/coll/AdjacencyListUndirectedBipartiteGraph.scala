package me.yuhuan.gossip.coll

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
trait AdjacencyListUndirectedBipartiteGraph[K1, K2, D] extends UndirectedBipartiteGraph[K1, K2, D] {

  def adjMap1: Map[K1, Set[K2]]
  def adjMap2: Map[K2, Set[K1]]

  def adjacentKeys1(k: K1): Set[K2] = adjMap1(k)
  def adjacentKeys2(k: K2): Set[K1] = adjMap2(k)

}

object AdjacencyListUndirectedBipartiteGraph {

  def apply[K1, K2, D](_adjMap1: Map[K1, Set[K2]], _adjMap2: Map[K2, Set[K1]], data: Map[(K1, K2), D]): AdjacencyListUndirectedBipartiteGraph[K1, K2, D] = {
    new AdjacencyListUndirectedBipartiteGraph[K1, K2, D] {
      def adjMap1: Map[K1, Set[K2]] = _adjMap1
      def adjMap2: Map[K2, Set[K1]] = _adjMap2
      def dataAt(k1: K1, k2: K2): Option[D] = data.get(k1 -> k2)
      def keys1: Set[K1] = _adjMap1.keySet
      def keys2: Set[K2] = _adjMap2.keySet
    }
  }

}