package org.inca.diff.json

import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestJson extends AnyFlatSpec with Matchers {

  def compareAndApply(src: Js, dest: Js): Assertion = {
    val patch = src.compareTo(dest)
    println(patch)
    src.applyPatch(patch) should be (Some(dest))
  }

  def parse(s: String): Js = fastparse.parse(s, Parser.jsonExpr(_)).get.value

  val doc1: String = """
    |{
    |  "firstName": "John",
    |  "lastName": "Smith",
    |  "age": 25,
    |  "address": {
    |      "streetAddress": "21 2nd Street",
    |      "city": "New York",
    |      "state": "NY",
    |      "postalCode": 10021
    |  },
    |  "phoneNumbers": [
    |      {
    |          "type": "home",
    |          "number": "212 555-1234"
    |      },
    |      {
    |          "type": "fax",
    |          "number": "646 555-4567"
    |      }
    |  ]
    |}
    |""".stripMargin
  val doc2: String = """
    |{
    |  "firstName": "John",
    |  "lastName": "Smith",
    |  "age": 25,
    |  "address": {
    |      "streetAddress": "Main Street",
    |      "city": "New York",
    |      "state": "NY",
    |      "postalCode": 10059
    |  },
    |  "phoneNumbers": [
    |      {
    |          "type": "home",
    |          "number": "212 555-1234"
    |      },
    |      {
    |          "type": "fax",
    |          "number": "646 555-4569"
    |      }
    |  ]
    |}
    |""".stripMargin
  val doc3: String = """
    |{
    |  "firstName": "John",
    |  "lastName": "Smith",
    |  "age": 25,
    |  "addresses": [
    |    { "address": {
    |        "streetAddress": "Main Street",
    |        "city": "New York",
    |        "state": "NY",
    |        "postalCode": 10059,
    |        "country": "USA"
    |    } },
    |    { "address": {
    |        "streetAddress": "Oxford Lane",
    |        "city": "London",
    |        "postalCode": 99188,
    |        "country": "United Kingdom"
    |    } }
    |  ],
    |  "phoneNumbers": [
    |      {
    |          "type": "home",
    |          "number": "212 555-1234"
    |      },
    |      {
    |          "type": "fax",
    |          "number": "646 555-4569"
    |      }
    |  ]
    |}
    |""".stripMargin

  "json diff" should "work" in {
    compareAndApply(parse(doc1), parse(doc1))
    compareAndApply(parse(doc1), parse(doc2))
    compareAndApply(parse(doc2), parse(doc3))
  }


}
