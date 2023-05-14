package collections

import scala.collection.mutable.ListBuffer
import scala.util.Random

object collections_experiment {

  case class Experiment(balls: ListBuffer[Ball]) {
    def isRandomBallWhite: Boolean = {
      val firstTry = getBall.color == WhiteColor
      val secondTry = getBall.color == WhiteColor
      firstTry || secondTry
    }

    private def getBall = balls.remove(Random.nextInt(balls.size))
  }

  case class Ball(color: Color)

  sealed trait Color

  case object WhiteColor extends Color
  case object BlackColor extends Color

}
