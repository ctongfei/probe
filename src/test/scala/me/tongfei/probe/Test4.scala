package me.tongfei.probe

/**
  * @author Tongfei Chen (ctongfei@gmail.com).
  */
object Test4 extends App {

  val fWords = Featurizer.count("words") { (s: String) =>
    print(s"!$s")
    s.split(' ')
  }

  val fCapitalWords = Featurizer.count("capitalizedwords") { (s: String) =>
    s.split(' ').map(_.toUpperCase)
  }

  val f1gram = Featurizer.count("1gram") { (s: String) =>
    s.sliding(1).toSeq
  }

  val f2gram = Featurizer.count("2gram") { (s: String) =>
    s.sliding(2).toSeq
  }

  val fShift = Featurizer.count("shifted") { (s: String) =>
    println(s"@$s")
    s.map(c => (c + 1).toChar.toString)
  }

  val s = "John killed Mary"
  val f = fWords >>> (fShift ++ fCapitalWords) >>> (f1gram ++ f2gram)
  val fs = f(s)
  val fv = FeatureVector.from(fs)

  val bp = 0
}
