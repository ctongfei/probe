package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object dsl {

  implicit class string2feature(val g: String) extends AnyVal {
    def ~(v: Any): Feature = Feature(g, v.toString)
    def $(w: Double) = Feature(g, "", w)
  }

}
