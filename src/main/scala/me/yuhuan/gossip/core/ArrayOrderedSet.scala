package me.yuhuan.gossip.core

import scala.reflect.ClassTag

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class ArrayOrderedSet[X: ClassTag](xs: Array[X])(implicit O: Ordering[X]) extends OrderedSet[X] {

  def ordering: Ordering[X] = O

  val data = ArrayOrderedSet.sort(xs.toSet.toArray)

  def length: Int = data.length

  def apply(idx: Int): X = data(idx)

  def iterator: Iterator[X] = data.iterator
}

object ArrayOrderedSet {

  /**
    * A QuickSort implementation from
    * <a href="http://www.scala-lang.org/docu/files/ScalaByExample.pdf#page=10" >Odersky 2014</a>.
    */
  def sort[X: ClassTag](xs: Array[X])(implicit O: Ordering[X]) : Array[X] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        sort(xs.filter(x => O.gt(pivot, x))),
        xs.filter(x => O.equiv(pivot, x)),
        sort(xs.filter(x => O.lt(pivot, x)))
      )
    }
  }

  def apply[X: ClassTag](xs: X*)(implicit O: Ordering[X]): ArrayOrderedSet[X] = new ArrayOrderedSet[X](xs.toArray)
}
