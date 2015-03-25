package textRank

import math.{log, abs}
import breeze.linalg._

object TextRank {
  def similarity(sentenceA: Array[String], sentenceB: Array[String]): Double = 
    sentenceA.count( x => sentenceA.contains(x) && sentenceB.contains(x))/
    (log(sentenceA.size) + log(sentenceB.size))
    
  def createGraphMatrix(sentences: Array[Array[String]]): DenseMatrix[Double] = {
    val n = sentences.size
    print(n)
    val m = DenseMatrix.zeros[Double](n,n)
    val d = 0.85
    var i = 0
    var j = 0
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        val outgoinWeights = sentences.foldLeft(0.0)((b, a) => b + similarity(sentences(j), a))
        if (outgoinWeights == 0.0) {
          m(i, j) = (1-d)/n
        } else {
          m(i,j) = (1-d)/n + d*similarity(sentences(j), sentences(i))/outgoinWeights
        }
      }
    }
    return m
  }
  
  def findEigen(matrix: DenseMatrix[Double], precision: Double): DenseVector[Double] = {
    var vector = DenseVector.zeros[Double](matrix.cols)
    vector(0) = 1
    var oldVector = DenseVector.zeros[Double](matrix.cols)
    var it = 0
    while(norm(vector-oldVector) > precision) {
      oldVector = vector
      vector = matrix*vector
      it = it + 1
    }
    return vector
  }

  def rank(text: String, toTake: Int) = {
    val sentences = StringParser.sentences(text)
    val wordsFromSentences = StringParser.wordsFromSentences(text)
    if (toTake > sentences.size) {
      val toReallyTake = sentences.size
    } else {
      val toReallyTake = toTake
    }
    val ranks: Array[Double] = findEigen(createGraphMatrix(wordsFromSentences), 0.01).data
  //  ranks.map(x => print(x))
    val sentencesOrderRank = (sentences zipWithIndex) zip ranks
    val sOR = sentencesOrderRank.map(x => Tuple3(x._1._1, x._1._2, x._2)).sortWith((a,b) => a._3 > b._3)
    (sOR take toTake).sortWith((a,b) => a._2 < b._2)
  }
}
