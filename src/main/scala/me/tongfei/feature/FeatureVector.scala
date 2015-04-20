package me.tongfei.feature

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class FeatureVector(val alphabet: Alphabet) {

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

  def pairs = data.iterator

  /**
   * Returns the sum of two feature vectors.
   * @param that
   * @return
   */
  def +(that: FeatureVector): FeatureVector = {
    if (this.alphabet ne that.alphabet) throw new Exception()
    val res = new FeatureVector(alphabet)
    for (i ← this.data.keySet union that.data.keySet)
      res.data(i) = this.data(i) + that.data(i)
    res
  }

  /**
   * Returns the difference of two feature vectors.
   * @param that
   * @return
   */
  def -(that: FeatureVector): FeatureVector = {
    if (this.alphabet ne that.alphabet) throw new Exception()
    val res = new FeatureVector(alphabet)
    for (i ← this.data.keySet union that.data.keySet)
      res.data(i) = this.data(i) - that.data(i)
    res
  }

  /**
   * Scales this feature vector by a constant.
   * @param k
   * @return
   */
  def *(k: Double): FeatureVector = {
    val res = new FeatureVector(alphabet)
    for (i ← this.data.keys)
      res.data(i) = this.data(i) * k
    res
  }

  /**
   * Returns the dot product of two feature vectors.
   * @param that
   * @return this . that
   */
  def dot(that: FeatureVector): Double = {
    if (this.alphabet ne that.alphabet) throw new Exception()
    var res = 0.0
    for (i ← this.data.keys)
      res += this.data(i) * that.data(i)
    res
  }

  /** Adds the specific list of features to this feature vector. */
  def <<=(fs: FeatureList) = {
    for (f ← fs) {
      data(alphabet(f.group + "~" + f.value)) += f.weight
    }
  }

  /** Adds the specific feature to this feature vector. */
  def <<=(f: Feature) = {
    data(alphabet(f.group + "~" + f.value)) += f.weight
  }

  /** Converts this feature vector to its text representation. */
  override def toString = {
    data.toArray.map{case (k, v) => (alphabet.get(k), v)}.sortBy(_._1).map{case (k, v) => s"$k:$v"}.mkString(" ")
  }

  /** Returns the alphabetized (internal sparse vector). */
  def alphabetize = data

}

object FeatureVector {

  /**
   * Creates an empty feature vector.
   */
  def empty(alphabet: Alphabet) = new FeatureVector(alphabet)

  /**
   * Converts an iterable list of features into a feature vector.
   */
  def apply(alphabet: Alphabet)(list: FeatureList) = {
    val r = new FeatureVector(alphabet)
    r <<= list
    r
  }

  /**
   * Reads a feature vector string. ("word~15:2.0 word~0:1.0 length:4.0")
   * @param alphabet A feature alphabet
   * @param s Feature vector string
   */
  def read(alphabet: Alphabet)(s: String) = {
    import dsl._
    val fv = FeatureVector.empty(alphabet)
    for (f ← s.split("\\s+")) {
      val tokens = f.split(":")
      val w = tokens(1).toDouble
      val gv = tokens(0).split("~")
      if (gv.length == 2) {
        val g = gv(0)
        val v = gv(1)
        fv <<= (g ~ v $ w)
      }
      else if (gv.length == 1) {
        val g = gv(0)
        fv <<= (g $ w)
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
    val fv = FeatureVector.empty(alphabet)
    for (f ← s.split("\\s+")) {
      val tokens = f.split(":")
      val w = tokens(1).toDouble
      val gv = tokens(0).toInt
      fv.data(gv) += w
    }
    fv
  }

}