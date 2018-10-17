package bowling

import scala.annotation.tailrec
import scala.util.Random

object Bowling extends App {

  val rnd = Random
  val frames = Game.play(rnd,9)
  println("La liste des manches : "+frames._1 + frames._2)
  val finalScore = Game.calculationScore(frames._1,frames._2)
  println("Score final: " +finalScore)
}

object Game{

  /**
    * Calcule le score d'une partie final en prenant en compte toute les frames (manches)
    * @param frames la liste de toute les manches de la partie. chaque manche contient 2 essais
    * @param lastFrame la derniere manche de la partie contenant possiblement 3 essais
    * @param score le score evoluant en fonction du resultat de chaque manche
    * @return le score final
    */
  def calculationScore(frames : List[(Int,Int)], lastFrame : (Int,Int,Int), score : Int = 0): Int = {
    try{

      if(frames.size > 2){
        val frame = frames.head
        if(frame._1 == 10){
          val nextFrame = frames.tail.head
          if (nextFrame._1 == 10){
            val nextNextFrame = frames.tail.tail.head
            calculationScore(frames.tail, lastFrame,score+frame._1+nextFrame._1+nextNextFrame._1 )
          }else{
            calculationScore(frames.tail, lastFrame,score+frame._1+nextFrame._1+nextFrame._2 )
          }
        }else if((frame._1+frame._2)==10){
          val nextFrame = frames.tail.head
          calculationScore(frames.tail, lastFrame,score+frame._1+frame._2+nextFrame._1 )
        }else{
          calculationScore(frames.tail, lastFrame,score+frame._1+frame._2 )
        }
      }else if(frames.size == 2){
        val frame = frames.head
        if(frame._1 == 10){
          val nextFrame = frames.tail.head
          if (nextFrame._1 == 10){
            val nextNextFrame = lastFrame
            calculationScore(frames.tail, lastFrame,score+frame._1+nextFrame._1+nextNextFrame._1 )
          }else{
            calculationScore(frames.tail, lastFrame,score+frame._1+nextFrame._1+nextFrame._2 )
          }
        }else if((frame._1+frame._2)==10){
          val nextFrame = frames.tail.head
          calculationScore(frames.tail, lastFrame,score+frame._1+frame._2+nextFrame._1 )
        }else{
          calculationScore(frames.tail, lastFrame,score+frame._1+frame._2 )
        }
      }else if(frames.size == 1){
        val frame = frames.head
        if(frame._1 == 10){
          val nextFrame = lastFrame
          calculationScore(frames.tail, lastFrame,score+frame._1+nextFrame._1+nextFrame._2 )
        }else if((frame._1+frame._2)==10){
          val nextFrame = lastFrame
          calculationScore(frames.tail, lastFrame,score+frame._1+frame._2+nextFrame._1 )
        }else{
          calculationScore(frames.tail, lastFrame,score+frame._1+frame._2 )
        }
      }else{
        if( ((lastFrame._1+ lastFrame._2) == 10) || (lastFrame._1 == 10)) score+lastFrame._1+lastFrame._2+lastFrame._3
        else score+lastFrame._1+lastFrame._2
      }
    }catch{
      case _:Exception => 0
    }
  }

  /**
    * Crée une liste de manches fictives contenant toujours la meme valeur
    * @param pin la valeur à integrer dans les manches
    * @return retourne la liste de manche et la manche final
    */
  def createListFrame(pin : Int) : (List[(Int,Int)],(Int,Int,Int)) = {
    if(pin < 6 || pin == 10){
      pin match{
        case 10 => (List.fill(9)((10,0)),(10,10,10))
        case _ =>  (List.fill(9)((pin,pin)),(pin,pin,pin))
      }
    }else{
      (List.fill(9)((0,0)),(0,0,0))
    }

  }

  /**
    * Crée une liste de manche contenant 2 tir aléatoire
    * @param rnd l'element random
    * @param numberOfFrame le nombre de manche restant à créer
    * @param newList la liste contenant les manches alétoires ajoutés
    * @return retourne une liste contenant les manches + la derniere manche contenant les 3 essais possibles
    */
  @tailrec
  def play(rnd: Random, numberOfFrame : Int, newList : List[(Int,Int)] = List()):(List[(Int,Int)],(Int,Int,Int)) = {

      if(numberOfFrame == 0){
        val try1 = rnd.nextInt(11)
        var try2 = 0
        if(try1 == 10) try2 = rnd.nextInt(11)
        else {
          val left = 11 - try1
          try2 = rnd.nextInt(left)
        }


        if((try1 == 10) || (try1+try2 == 10)){
          val try3 = rnd.nextInt(11)
          (newList,(try1,try2,try3))
        }else{
          (newList,(try1,try2,0))
        }

      }else{
        val try1 = rnd.nextInt(11)
        val left = 11 - try1
        val try2 = rnd.nextInt(left)
        play(rnd,numberOfFrame - 1,(try1,try2)::newList)
      }
  }


}