package me.yuhuan.gossip.core

import me.yuhuan.gossip.structure.Var

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class VarSet(val vs: Array[Var])(implicit O: Ordering[Var]) extends ArrayOrderedSet[Var](vs) { self =>

  /**
    * Domain sizes of variables.
    */
  protected val domainSizes = self.toArray.map(_.domainSize)

  /**
    * Say I have variable domain sizes: 2,3,2,3
    * This tmp array will be 36,18,6,3,1
    * where the first element is the domain size of the factor
    * and the rest are variable strides.
    * <img src="http://people.cs.pitt.edu/~yuhuan/docs/img/factor-strides-scan-right.png" />
    */
  protected val tmp = domainSizes.scanRight(1)(_*_)

  /**
    * Domain size of this factor. Equals to `domainSizes.fold(1)(_*_)`
    */
  val domainSize = tmp.head

  val strides = tmp.tail

  def configIdxToConfig(configIdx: Int): Seq[Int] = {
    val config = Array.ofDim[Int](self.length)
    var remainder = configIdx
    for (i <- config.indices) {
      config(i) = remainder / strides(i)
      remainder -= config(i) * strides(i)
    }
    config
  }

  def configToConfigIdx(config: Seq[Int]): Int = {
    (strides zip config).map { case (a, b) => a * b }.sum
  }

  // IndexFor
  // Assumes that the ordering of vars in vs is the same as this factor
  def configIdcsWrt(vs: OrderedSet[Var]): Iterable[Int] = {
    new Iterable[Int] {
      def iterator: Iterator[Int] = new Iterator[Int] {

        var _index = 1l
        val _sum = Array.ofDim[Long](vs.length)
        val _state = Array.ofDim[Int](vs.length)
        val _ranges = Array.ofDim[Int](vs.length)

        var sum = self.map(_.domainSize).product

        var cur = 0
        var jj = 0

        for (i <- self) {
          sum /= i.domainSize
          while (jj < vs.size && self.ordering.compare(vs(jj), i) <= 0) {
            val j = vs(jj)
            _ranges(cur) = j.domainSize
            _sum(cur) = if (self.ordering.equiv(i, j)) sum else 0
            cur += 1
            jj += 1
          }
        }

        while (jj < vs.size) {
          val j = vs(jj)
          _ranges(cur) = j.domainSize
          _sum(cur) = 0l
          cur += 1
          jj += 1
        }

        _index = 0
        assert(cur == _state.length)

        def hasNext: Boolean = _index >= 0

        def next(): Int = {
          import scala.util.control.Breaks._

          val curIndex = _index
          if (_index >= 0) {
            var i = _state.length - 1

            breakable {
              while (i >= 0) {
                _index += _sum(i)
                _state(i) += 1
                if (_state(i) < _ranges(i)) break()
                _index -= _sum(i) * _ranges(i)
                _state(i) = 0
                i -= 1
              }
            }

            if (i == -1) _index = -1
          }

          curIndex.toInt
        }

      }
    }
  }


  override def toString(): String = s"{${self.vs.map(_.name).mkString(",")}}"

}


object VarSet {
  def apply(vars: Var*): VarSet = new VarSet(vars.toArray)
}