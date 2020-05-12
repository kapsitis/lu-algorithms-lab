package lv.rbs.ds.lab03

import scala.collection.mutable.ArrayBuffer

class KMPmatcher(var pattern: String) {
  val m = pattern.length

  def getPrefixFun(arg:String): List[Int] = {
    val result = ArrayBuffer.empty[Int]
    for (i <- 0 to m) {
      result += 0
    }
    result(0) = -1
    result(1) = 0
    var k = 0
    for (q <- 2 to m) {
      while (k > 0 && arg.charAt(k) != arg.charAt(q-1)) {
        k = result(k)
      }
      if (arg.charAt(k) == arg.charAt(q-1)) {
        k = k+1
      }
      result(q) = k
    }
    result.toList
  }


  def getPrefixFun(): List[Int] = {
    return getPrefixFun(pattern)
  }


  def findAllIn(text: CharSequence): Iterator[Int] = {
    var result = ArrayBuffer.empty[Int]
    val n = text.length()
    val pi = getPrefixFun(pattern)
    var k = 0
    for (i <- 0 to n-1) {
      //println("i = " + i)
      while (k > 0 && pattern.charAt(k) != text.charAt(i)) {
        k = pi(k)
      }
      //println("PRE i,k = " + i + "," + k)
      if (pattern.charAt(k) == text.charAt(i)) {
        k = k + 1
      }
      //println("POST i,k = " + i + "," + k)
      if (k == m) {
        //println("i,m = " + i + "," + m)
        result += (i-m+1)
        k = pi(k)
      }
    }
    return result.iterator
  }


  def findAllSearchSteps(text: CharSequence): List[(Int,Int,Int,Int)] = {

    var result = ArrayBuffer.empty[(Int,Int,Int,Int)]
    val n = text.length()
    val pi = getPrefixFun(pattern)
    var k = 0
    var kStart = 0
    for (i <- 0 to n-1) {
      //println("### i,k,kStart = " + i + "," + k + ","+ kStart)
      while (k > 0 && pattern.charAt(k) != text.charAt(i)) {

        val newItem = Tuple4(i-k, kStart, k, 0)
        //println("i,k,kStart = " + i + "," + k + "," + kStart + "," + newItem)
        result += newItem
        k = pi(k)
        kStart = k
        //println("i,k,kStart = " + i + "," + k + "," + kStart)
      }
      if (pattern.charAt(k) == text.charAt(i)) {
        k = k + 1
      } else {
        if (k == 0) {
          kStart = 0
          val newItem = Tuple4(i-k, 0, 0, 0)
          result += newItem
        }
      }
      if (k == m) {
        val newItem = Tuple4(i-m+1, kStart, k-1, 1)
        //println("MATCH i,k,kStart = " + i + "," + k + ","+ kStart + "," + newItem)
        result += newItem
        //println("MATCH i,k,kStart = " + i + "," + k+ ","+ kStart)
        k = pi(k)
        kStart = k
      }
    }
    return result.toList
  }


  def toJson(text: CharSequence): String = {
    val transcript = new SearchTranscript("KMP", pattern, text, findAllSearchSteps(text))
    transcript.getJson()
  }
}
