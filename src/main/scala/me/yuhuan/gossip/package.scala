package me.yuhuan

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
package object gossip {

  def invert[X, Y](outMap: Map[Y, Set[X]]): Map[X, Set[Y]] = {
    val arcs = for (y <- outMap.keys; x <- outMap(y)) yield x -> y
    arcs.groupBy(_._1).map { case (x, ps) => x -> ps.map(_._2).toSet }
  }


}
