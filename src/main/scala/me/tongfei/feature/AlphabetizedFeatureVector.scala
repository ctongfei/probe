package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class AlphabetizedFeatureVector(val alphabet: Alphabet) {

  private val data = new SparseVector

  def size = alphabet.size

  def apply(feature: String): Double = {
    if (alphabet contains feature)
      data(alphabet(feature))
    else 0.0
  }

  def update(feature: String, x: Double) = {
    data(alphabet(feature)) = x
  }

  /** Creates a copy of this feature vector with the same alphabet. */
  def copy: AlphabetizedFeatureVector = {
    val n = new AlphabetizedFeatureVector(this.alphabet)
    n.data ++= this.data
    n
  }

  def pairs = data.asInstanceOf[Iterable[(Int, Double)]]

  /** Returns the sum of two feature vectors. */
  def +(that: AlphabetizedFeatureVector): AlphabetizedFeatureVector = {
    if (this.alphabet ne that.alphabet) throw new Exception()
    val res = new AlphabetizedFeatureVector(alphabet)
    for (i ← this.data.keySet union that.data.keySet)
      res.data(i) = this.data(i) + that.data(i)
    res
  }

  /** Returns the difference of two feature vectors. */
  def -(that: AlphabetizedFeatureVector): AlphabetizedFeatureVector = {
    if (this.alphabet ne that.alphabet) throw new Exception()
    val res = new AlphabetizedFeatureVector(alphabet)
    for (i ← this.data.keySet union that.data.keySet)
      res.data(i) = this.data(i) - that.data(i)
    res
  }

  /** Scales this feature vector by a constant. */
  def *(k: Double): AlphabetizedFeatureVector = {
    val res = new AlphabetizedFeatureVector(alphabet)
    for (i ← this.data.keys)
      res.data(i) = this.data(i) * k
    res
  }

  /** Returns the dot product of two feature vectors. */
  def dot(that: AlphabetizedFeatureVector): Double = {
    if (this.alphabet ne that.alphabet) throw new Exception()
    var res = 0.0
    for (i ← this.data.keys)
      res += this.data(i) * that.data(i)
    res
  }

  def cosSimilarity(that: AlphabetizedFeatureVector): Double = (this dot that) / this.l2Norm / that.l2Norm

  def l2Norm: Double = {
    var res = 0.0
    for (i ← this.data)
      res += i._2 * i._2
    math.sqrt(res)
  }

  def l2Normalize: AlphabetizedFeatureVector = this * (1.0 / this.l2Norm)

  def maxNorm: Double = this.data.values.max

  def maxNormalize: AlphabetizedFeatureVector = this * (1.0 / this.maxNorm)

  /** Adds the specific list of features to this feature vector. */
  def <<=(fs: FeatureList) = {
    for (f ← fs) {
      data(alphabet(f._1.name)) += f._2
    }
  }

  /** Adds the specific feature to this feature vector. */
  def <<=(f: (Feature, Double)) = {
    data(alphabet(f._1.name)) += f._2
  }

  /** Adds a binary feature to this feature vector. */
  def <<=(f: Feature) = {
    data(alphabet(f.name)) += 1.0
  }

  def toFeatureList: FeatureList = FeatureList.ofIterable(pairs.view.map { case (k, v) => Feature(alphabet.get(k)) → v })

  /** Converts this feature vector to its text representation. */
  override def toString = {
    data.toArray.map{case (k, v) => (alphabet.get(k), v)}.sortBy(_._1).map{case (k, v) => s"$k:$v"}.mkString(" ")
  }

  /** Returns the alphabetized (internal) sparse vector. */
  def internal = data

}

object AlphabetizedFeatureVector {

  /** Creates an empty feature vector. */
  def empty(alphabet: Alphabet) = new AlphabetizedFeatureVector(alphabet)

  /** Converts an iterable list of features into a feature vector. */
  def apply(alphabet: Alphabet)(list: FeatureList) = {
    val r = new AlphabetizedFeatureVector(alphabet)
    r <<= list
    r
  }

  /**
   * Reads a feature vector string. ("word~15:2.0 word~0:1.0 length:4.0")
   * @param alphabet A feature alphabet
   * @param s Feature vector string
   */
  def read(alphabet: Alphabet)(s: String) = {
    val fv = AlphabetizedFeatureVector.empty(alphabet)
    for (f ← s.split("\\s+")) {
      val tokens = f.split(":")
      val w = tokens(1).toDouble
      val gv = tokens(0).split("~")
      if (gv.length == 2) {
        val g = gv(0)
        val v = gv(1)
        fv <<= Feature(g, v) → w
      }
      else if (gv.length == 1) {
        val g = gv(0)
        fv <<= Feature(g) → w
      }
      else throw new NumberFormatException
    }
    fv
  }

  /**
   * Reads an alphabetized feature vector string. ("1:3.0 32:1.0 677:3.41")
   * @param alphabet A feature alphabet
   * @param s Alphabetized feature vector string
   */
  def readAlphabetized(alphabet: Alphabet)(s: String) = {
    val fv = AlphabetizedFeatureVector.empty(alphabet)
    for (f ← s.split("\\s+")) {
      val tokens = f.split(":")
      val w = tokens(1).toDouble
      val gv = tokens(0).toInt
      fv.data(gv) += w
    }
    fv
  }

}
