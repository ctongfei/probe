package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object AlphabetTest extends App {

  val alphabet = Alphabet()

  alphabet("my")
  alphabet.register("life")
  alphabet("is")
  alphabet("you")
  alphabet.register("query")

  alphabet.save("test")

  val alphabet2 = Alphabet.load("test")

}
