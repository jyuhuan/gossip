package me.yuhuan.gossip.core

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
case class Arc[From, To](from: From, to: To)
object Arc {
  type ~>[From, To] = Arc[From, To]
}