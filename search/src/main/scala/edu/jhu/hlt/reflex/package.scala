package edu.jhu.hlt

/**
 * @author Tongfei Chen
 */
package object reflex {
  implicit class StringMatch(ctx: StringContext) {

    /** Enables the "sm" string interpolator. */
    object sm {

      def unapplySeq(s: String): Option[List[String]] = {
        val rx = ctx.parts.map(_.replace("(", "\\(").replace(")", "\\)")).mkString("(.*)")
        rx.r.unapplySeq(s)
      }

    }

  }
}
