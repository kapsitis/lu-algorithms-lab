package lv.rbs.ds.lab03

import org.scalatest.Matchers.include
import net.liftweb.json._
import org.scalatest._

class KMPpublicTest  extends FunSuite {
  test("Prefix function for abab") {
    val matcher = new KMPmatcher("abab")
    assert(matcher.getPrefixFun().toList === List(-1,0,0,1,2))
  }

  test("Prefix function for aabaab") {
    val matcher = new KMPmatcher("aabaab")
    assert(matcher.getPrefixFun().toList === List(-1,0,1,0,1,2,3))
  }


  test("Prefix function for ABCDABD") {
    val matcher = new KMPmatcher("ABCDABD")
    assert(matcher.getPrefixFun().toList === List(-1,0,0,0,0,1,2,0))
  }

  test("Function findAllIn right at start returns 0") {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("abc").toList
    assert(result === List(0))
  }



  test("findAllIn returns 3 items, if they exist") {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    assert(result === List(2, 6, 13))
  }


  test("Return empty findAllIn iterator, if none exist") {
    val matcher = new KMPmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    assert(result === List())
  }

  // **************************************************************************
  // Check if the pattern preprocessing functions are correct
  // **************************************************************************
  test("return prefixFun") {
    val myPattern = "ABCDABD"
    val matcher = new KMPmatcher(myPattern)
    assert(matcher.getPrefixFun() === List(-1,0,0,0,0,1,2,0))
  }


  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  test("Return JSON string with the right algorithm type") {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    assert (result.indexOf("KMP") >= 0)
  }


  test("Return 3 correct string fields") {
    val myPattern = "ABCDABD"
    val matcher = new KMPmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    assert(aa  === "KMP")
    val pp = (json \ "pattern").extract[String]
    assert(pp  === myPattern)
    val tt = (json \ "text").extract[String]
    assert(tt === myText)
  }

  test("Count character comparisons correctly") {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val comparisons = (json \ "comparisons").extract[Int]
    assert(comparisons === 27)
  }

  test("return correct steps 0, 1 and 6") {
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
    assert(resultSteps(0)("offset") === "0")
    assert(resultSteps(0)("start")  === "0")
    assert(resultSteps(0)("end") === "3")
    assert(resultSteps(0).keySet.contains("match")  === false)

    assert(resultSteps(1)("offset") === "3")
    assert(resultSteps(1)("start") === "0")
    assert(resultSteps(1)("end") === "0")
    assert(resultSteps(1).keySet.contains("match") === false)

    assert(resultSteps(6)("offset") === "15")
    assert(resultSteps(6)("start") === "2")
    assert(resultSteps(6)("end") === "6")
    assert(resultSteps(6)("match") === "true")
  }

  /*
  test("Return all the right steps") {
    val expected = List(
      (0, 0, 3, 0), (3, 0, 0, 0), (4, 0, 6, 0),
      (8, 2, 2, 0), (10, 0, 0, 0), (11, 0, 6, 0),
      (15, 2, 6, 1), (22, 0, 0, 0)
    )
    val matcher = new KMPmatcher("ABCDABD")
    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    assert(result === expected)
  }
*/

  // **************************************************************************
  // The following tests are considred "private"
  // **************************************************************************
  test("Prefix function for a 4th order Gray string") {
    val matcher = new KMPmatcher("ABACABADABACABA")
    assert(matcher.getPrefixFun().toList === List(-1,0,0,1,0,1,2,3,0,1,2,3,4,5,6,7))
  }

  test("Prefix function for a Thue-Morse sequence") {
    val matcher = new KMPmatcher("0110100110010110")
    assert(matcher.getPrefixFun().toList === List(-1,0,0,0,1,2,1,1,2,3,4,1,2,1,2,3,4))
  }



}
