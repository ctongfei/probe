package me.tongfei.feature

import scala.collection._

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.4.0
  */
class FeatureVector {

  private var groups = new mutable.ArrayBuffer[FeatureGroup[Any]]()

  def +=(fg: FeatureGroup[Any]) = groups += fg

  def group(name: String) = groups.find(_.name == name).get

  def features = groups.view.flatMap(_.features)

  def toStringFeatureVector: StringFeatureVector = {
    val sfv = new StringFeatureVector
    groups foreach sfv.<<=
    sfv
  }

}

object FeatureVector {

  def apply(fgs: FeatureGroup[Any]*): FeatureVector = {
    val fv = new FeatureVector
    fv.groups ++= fgs
    fv
  }

}