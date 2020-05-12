package lv.rbs.ds.lab03

import org.scalatest._
import net.liftweb.json._
import org.scalatest.Matchers._

class BMpublicTest  extends FunSuite {
  test("good suffix in CABAB") {
    val matcher = new BMmatcher("CABAB")
    val result = matcher.getGoodSuffixFun()
    assert(result === List(5,5,5,2,2,1))
  }

  test("good suffix in ABBAB") {
    val matcher = new BMmatcher("ABBAB")
    val result = matcher.getGoodSuffixFun()
    assert(result === List(3,3,3,3,2,1))
  }

  test("good suffix in CBAAB") {
    val matcher = new BMmatcher("CBAAB")
    val result = matcher.getGoodSuffixFun()
    assert(result === List(5,5,5,5,3,1))
  }

  test("return 3 items in findAllIn iterator, if they exist") {
    val matcher = new BMmatcher("ab")
    val result = matcher.findAllIn("stab, about, above").toList
    assert(result === List(2, 6, 13))
  }

  test("Bad char function in CABAB") {
    val matcher = new BMmatcher("CABAB")
    val result = matcher.getBadCharFun()
    assert(result === List(('A',3),('B',4),('C',0)))
  }




  test("Return empty findAllIn iterator, if none exist") {
    val matcher = new BMmatcher("ab")
    val result = matcher.findAllIn("xxxx, yyyyy, zzzzz").toList
    assert(result === List())
  }

  test("Return 1 item in findAllIn iterator") {
    val matcher = new BMmatcher("ABBABAB")
    val result = matcher.findAllIn("ABAABABBBBABBBAAABBABAB").toList
    assert(result === List(16))
  }


  // **************************************************************************
  // Check if the pattern preprocessing functions are correct
  // **************************************************************************
  test("Return goodSuffixFun for ABCDABD") {
    val expected = List(7,7,7,7,7,7,3,1)
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    assert(matcher.getGoodSuffixFun() === expected)
  }

  test("Return badCharacterFun") {
    val expected:List[(Char,Int)] = List(('A',4),('B',5),('C',2),('D',6))
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    assert(matcher.getBadCharFun() === expected)
  }


  // **************************************************************************
  // Testing the returned JSON as a string.
  // **************************************************************************
  test("Return JSON string with the right algorithm type") {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    assert(result.indexOf("BM") >= 0)
  }

  // **************************************************************************
  // Testing the returned JSON as a parsed data structure.
  // **************************************************************************
  test("Return 3 correct string fields") {
    val myPattern = "ABCDABD"
    val matcher = new BMmatcher(myPattern)
    val myText = "ABC ABCDAB ABCDABCDABDE"
    val result = matcher.toJson(myText)
    val json = parse(result)
    implicit val formats = DefaultFormats
    val aa = (json \ "algorithm").extract[String]
    assert(aa === "BM")
    val pp = (json \ "pattern").extract[String]
    assert(pp === myPattern)
    val tt = (json \ "text").extract[String]
    assert(tt === myText)
  }

  test ("Return correct steps 0, 1 and 3") {
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.toJson("ABC ABCDAB ABCDABCDABDE")
    val json = parse(result)
    implicit val formats = DefaultFormats
    val resultSteps = (json \ "steps").extract[List[Map[String, String]]]
    assert(resultSteps(0)("offset") === "0")
    assert(resultSteps(0)("start") === "6")
    assert(resultSteps(0)("end") === "6")
    assert(resultSteps(0).keySet.contains("match") === false)

    assert(resultSteps(1)("offset") === "4")
    assert(resultSteps(1)("start") === "6")
    assert(resultSteps(1)("end")  === "6")
    assert(resultSteps(1).keySet.contains("match") === false)



    assert(resultSteps(2)("offset") === "11")
    assert(resultSteps(2)("start") === "6")
    assert(resultSteps(2)("end") === "6")
    assert(resultSteps(2).keySet.contains("match") === false)


    assert(resultSteps(3)("offset") === "15")
    assert(resultSteps(3)("start") === "6")
    assert(resultSteps(3)("end") === "0")
    assert(resultSteps(3)("match") === "true")
  }

  /*
  test("Return all the right steps") {
    val expected = List(
      (0, 6, 6, 0), (4, 6, 6, 0), (11, 6, 6, 0),
      (15, 6, 0, 1)
    )
    val matcher = new BMmatcher("ABCDABD")
    val result = matcher.findAllSearchSteps("ABC ABCDAB ABCDABCDABDE")
    assert(result === expected)
  }
  */

  test("Bad character function for a 4th order Gray string") {
    val matcher = new BMmatcher("ABACABADABACABA")
    assert(matcher.getBadCharFun() === List(('A',14), ('B',13), ('C',11), ('D',7)))
  }

  test("Bad character function for a Thue-Morse sequence") {
    val matcher = new BMmatcher("0110100110010110")
    assert(matcher.getBadCharFun() === List(('0',15), ('1',14)))
  }

  test("Good suffix function for a 4th order Gray string") {
    val matcher = new BMmatcher("ABACABADABACABA")
    assert(matcher.getGoodSuffixFun() === List(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 2, 1))
  }

  test("Good suffix function for a Thue-Morse sequence") {
    val matcher = new BMmatcher("0110100110010110")
    assert(matcher.getGoodSuffixFun() === List(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 6, 6, 3, 3, 1))
  }


}
