package lv.rbs.ds.lab03

object MatchMain {
  def main(args: Array[String]): Unit = {
    //val matcher = new KMPmatcher("ab")
    //val result = matcher.findAllIn("abc").toList

    /*
    val matcher = new KMPmatcher("ABCDABD")
    val pi = matcher.getPrefixFun()
    println("pi = " + pi)

    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    println("result = " + result)
*/

    val matcher = new BMmatcher("CABAB")
    val result = matcher.getGoodSuffixFun()
    println("result = " + result)
  }
}