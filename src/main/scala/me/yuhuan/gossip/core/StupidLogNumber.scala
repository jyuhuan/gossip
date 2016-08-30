package me.yuhuan.gossip.core

import math._

/**
  * @author Yuhuan Jiang (jyuhuan@gmail.com).
  */
class SlowLogNumber(logX: Double, val isPositive: Boolean) { self =>

  def +(that: SlowLogNumber): SlowLogNumber = {
    (self.isPositive, that.isPositive) match {
      case (false, false) => SlowLogNumber(log(self.value + that.value), isPositive = false)

      case (false, true)  =>
        if (abs(self.value) < abs(that.value)) SlowLogNumber(log(abs(that.value) - abs(self.value)), isPositive = true)
        else if (abs(self.value) > abs(that.value)) SlowLogNumber(log(abs(self.value) - abs(that.value)), isPositive = false)
        else SlowLogNumber(0.0)

      case (true, false)  =>
        if (abs(self.value) < abs(that.value)) SlowLogNumber(log(abs(that.value) - abs(self.value)), isPositive = false)
        else if (abs(self.value) > abs(that.value)) SlowLogNumber(log(abs(self.value) - abs(that.value)), isPositive = true)
        else SlowLogNumber(0.0)

      case (true, true)   => SlowLogNumber(log(self.value + that.value), isPositive = true)
    }
  }

  def -(that: SlowLogNumber): SlowLogNumber = ???

  def *(that: SlowLogNumber): SlowLogNumber = ???

  def /(that: SlowLogNumber): SlowLogNumber = ???

  def value = if (isPositive) exp(logX) else -exp(logX)

  override def toString: String = s"$value"

}

object SlowLogNumber {
  def apply(logX: Double, isPositive: Boolean): SlowLogNumber = new SlowLogNumber(logX, isPositive)
  def apply(x: Double): SlowLogNumber = new SlowLogNumber(log(abs(x)), x > 0)
}