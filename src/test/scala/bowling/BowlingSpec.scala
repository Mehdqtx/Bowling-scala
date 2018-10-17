package bowling

import bowling.Bowling.rnd
import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

class BowlingSpec extends FunSpec with Matchers {


  describe("Bowling score") {

      it("SCORE CALCULATION: should be 0 when all roll into gutter") {
        val frames = Game.createListFrame(0)
        assert(Game.calculationScore(frames._1,frames._2) == 0)
      }

      it("SCORE CALCULATION: should be 20 when all roll hit 1 pin") {
        val frames = Game.createListFrame(1)
        assert(Game.calculationScore(frames._1,frames._2) == 20)
      }

      it("SCORE CALCULATION: should be 40 when all roll hit 2 pin") {
        val frames = Game.createListFrame(2)
        assert(Game.calculationScore(frames._1,frames._2) == 40)
      }

      it("SCORE CALCULATION: should be 300 when all frame are strikes") {
        val frames = Game.createListFrame(10)
        assert(Game.calculationScore(frames._1,frames._2) == 300)
      }

      it("SCORE CALCULATION: should be 150 when all frame are spares") {
        val frames = Game.createListFrame(5)
        assert(Game.calculationScore(frames._1,frames._2) == 150)
      }

      it("SCORE CALCULATION: should be 240 when 9 strikes and in the tenth frame roll hit 0 pin") {
        val frames = Game.createListFrame(10)
        assert(Game.calculationScore(frames._1,(0,0,0)) == 240)
      }

      it("SCORE CALCULATION: should be 0 when 9 strikes and in the tenth frame roll hit 5 three times") {
        val frames = Game.createListFrame(10)
        assert(Game.calculationScore(frames._1,(5,5,5)) == 270)
      }

      it("SCORE CALCULATION: should be 48 when 9 frames hits 1 and in the tenth frame strikes 3 times") {
        val frames = Game.createListFrame(1)
        assert(Game.calculationScore(frames._1,(10,10,10)) == 48)
      }

    it("RANDOM PLAY: should be <=10 when we calculate the sum of rolls in a frame") {
      val rnd = Random
      val frames = Game.play(rnd,9)
      assert((frames._1.head._1 + frames._1.head._2) <= 10 )
    }
  }

}
