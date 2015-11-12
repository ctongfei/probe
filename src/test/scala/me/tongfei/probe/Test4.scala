package me.tongfei.probe

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object Test4 extends App {

  val fWords = Featurizer("words") { (s: String) =>
    print("!")
    FeatureGroup.count("words")(s.split(' '))
  }

  val fCapitalWords = Featurizer("capitalizedwords") { (s: String) =>
    FeatureGroup.count("capitalizedwords")(s.split(' ').map(_.toUpperCase))
  }

  val f1gram = Featurizer("1gram") { (s: String) =>
    FeatureGroup.count("1gram")(s.sliding(1).toSeq)
  }

  val f2gram = Featurizer("2gram") { (s: String) =>
    FeatureGroup.count("2gram")(s.sliding(2).toSeq)
  }

  val fShift = Featurizer("shifted") { (s: String) =>
    println(s"@$s")
    FeatureGroup.count("shifted")(s.map(c => (c + 1).toChar.toString))
  }

  val s = "John killed Mary"
  val f = fWords >>> ((fShift ++ fCapitalWords) >>> (f1gram ++ f2gram))
  val fs = f(s)
  val fv = FeatureVector.from(fs)

  val bp = 0
}
