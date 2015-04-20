package me.tongfei.feature


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class FeatureVector(val alphabet: Alphabet) {

  private val data = new SparseVector

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

  /**
   * Adds the specified features to this feature vector.
   * @param fs Features
   */
  def <<=(fs: FeatureList) = {
    for (f ← fs) {
      data(alphabet(f.group + "#" + f.value)) += f.weight
    }
  }

  def <<=(f: Feature) = {
    data(alphabet(f.group + "#" + f.value)) += f.weight
  }

  /**
   * Converts this feature vector to its text representation.
   */
  override def toString = {
    data.toArray.map{case (k, v) => (alphabet.get(k), v)}.sortBy(_._1).map{case (k, v) => s"$k:$v"}.mkString(" ")
  }

  def alphabetize = data

}

object FeatureVector {
  def empty(alphabet: Alphabet) = new FeatureVector(alphabet)
  def apply(alphabet: Alphabet)(list: FeatureList) = {
    val r = new FeatureVector(alphabet)
    r <<= list
    r
  }
}