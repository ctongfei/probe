## Probe
Discriminative feature-based information retrieval and an NLP feature engineering framework

Current version: **`0.8.0`**

If you are using it in an academic work, please cite
```tex
@inproceedings{chen2017discriminative,
  title={Discriminative information retrieval for question answering sentence selection},
  author={Chen, Tongfei and Van Durme, Benjamin},
  booktitle={Proceedings of EACL},
  year={2017}
}
```

#### Introduction
In natural language processing, tasks that require heavy feature engineering is often encountered. 
These code often contains a lot of boilerplate and ad-hoc tricks, making the code hard to maintain.

`Probe` provides a principled, functional way for doing feature extraction by heavy use of compositional 
operators that leads to an elegant embedded DSL for feature extraction.

An example of the DSL:
```scala
val fx = 
  SyntacticParse >>> (
    Predicate >>> (SyntacticHead ++ SemanticHead ++ FirstWord) ++
    Argument >>> (SemanticHead ++ LastWord)
  )
```

#### Feature and feature groups
First, what is a `Feature`? How is it defined in `Probe`?

Features, or feature vectors that we often see in natural language processing contains:
```scala
[1] namedEntityType~GPE   // the type of a named entity

[2] distance:5            // the distance between two words
   
[3] (
       hasWord~John
       hasWord~loves
       hasWord~Mary
    )                     // a group of bag-of-words features
   
[4] (
      tfIdf~avocado:7.4561
      ...
      tfIdf~is:1.1452
    )                     // a group of features that stores the tf-idf value of each word
    
[5] (
      emb~0:0.124
      emb~1:-0.786
      ...
      emb~99:0.443
    )                     // the embedding of a word
    
```
The `Feature[A]` type in `Probe` is designed to capture all these examples listed above. 
A `Feature` consists of the following parts:
```scala
    "name"       "key"        "value"
    tfIdf   ~   avocado   :   7.4561
```
The name must be of type `String`, the value `Double`, and the key can be of any type `A`.

A `FeatureGroup` is a group of features that share the same `name`, and the keys are of the same type. 
The examples `[3]`, `[4]` and `[5]` are all `FeatureGroup`s.

A basic `FeatureGroup` trait is defined as follows:
```scala
trait FeatureGroup[A] {
  def name: String
  def pairs: Iterable[(A, Double)]
```
There are different subtypes of the general trait `FeatureGroup`, e.g. `BinaryFeatureGroup`s etc.,
but a user shouldn't be worried about these: these are used internally to avoid unnecessary
memory use. (e.g. we don't have to store the value 1.0 in the weight of each feature in a `BinaryFeatureGroup`!)

#### Featurizers
A `Featurizer` is something that extracts **a single feature group** out of something. Essentially, `Featurizer[A, B]`
extends `(A => FeatureGroup[B])`. To create a featurizer, one can use the following six constructors for creating
different types of `Featurizer`:
 - A single categorical feature (example `[1]`):
 
   ```scala
   Featurizer.singleCategorical("name") { (a: A) => f(a) } // Extracts a single feature f(a) out of a.
   ```
 
- A single numerical feature (example `[2]`):
 
   ```scala
   Featurizer.singleNumerical("name") { (a: A) => f(a) } // Extracts a single feature "()" out of a with weight f(a).
   ```
 
- A group of binary features whose occurrences will be counted (example `[3]`):
  
   ```scala
   Featurizer.count("name") { (a: A) => f(a) } // f(a) is of type Iterable[B] that contains the list of features.
   ```

- A group of binary features whose occurrences will not be counted (only 0 or 1):

   ```scala
   Featurizer.binary("name") { (a: A) => f(a) } // f(a) is of type Iterable[B] that contains the list of features.
   ```

- A group of real-valued features (example `[4]`):

   ```scala
   Featurizer.realValued("name") { (a: A) => f(a) } // f(a) is of type Iterable[(B, Double)] which are (key, value) pairs.
   ```
   
- A real vector as a group (example `[5]`):

   ```scala
   Featurizer.realVector("name") { (a: A) => f(a) } // f(a) is of type Array[Double].
   ```

#### Compositional operators on featurizers
All these operators listed below will return a new featurizer: the original featurizer is not mutated.

| Operator                 | Description                                              |
|--------------------------|----------------------------------------------------------|
| `>>>` (`andThen`)        | Pipes through another featurizer.                        |
| `map(f)`                 | Transforms all keys by function f.                       |
| `filter(f)`              | Selects only the keys that satisfy predicate f.          |
| `topK(k)`                | Selects only the k largest-value features.               |
| `assignWeights(f)`       | Assigns each feature with key k with value f(k).         |
| `uniformWeight`          | Sets all values to 1.                                    |
| `binarize(t)`            | Selects only the features whose values are >= t.         |
| `×` (`cartesianProduct`) | Performs Cartesian product on two featurizers.           |
| `appendName(s)`          | Append the string s to the end of the name of the group. |

#### Feature similarity measures
Additionally, `Probe` supports the computation of similarities between two feature vectors. This similarity feature can also be used as a featurizer that extracts a similarity feature out of a pair of objects. Currently `Equality` (tests for equivalence of keys), `CosineSimilarity` and `JaccardSimilarity` are supported.

#### Feature extractor and the DSL
A `Featurizer` in `Probe` only extracts a group of the same name out of an object. We often need to compose different `Featurizer`s to get one big composite feature extractor.

In `Probe`, a `FeatureExtractor` is something that can extracts a **list of feature groups** (can have different names!) out of an object. It is defined in the following recursive way:
 - A `Featurizer` is a `FeatureExtractor`.
 - A list of `FeatureExtractor`s, when concatenated, is a `FeatureExtractor`. (use the operator `++`)
 - Two `FeatureExtractor`s that have the type parameter `[A, B]` and `[A, C]` respectively, can be composed to get a `FeatureExtractor`. (use the operator `>>>`).

Using the operators on `Featurizer`s and the compositional operators on `FeatureExtractor`s, an elegant DSL for feature extraction is ready for use.

Examples:
  For a pair of named entities, extracts simple features to see whether they corefer.
  ```scala
  val fx = 
    Equality(Verbatim) ++
    Jaccard(Verbatim >>> BagOfWords) ++
    Jaccard(Verbatim >>> BagOfWords >>> Letter3Gram) ++
    Equality(EntityType) ++
    Cosine(SentenceContext >>> BagOfWords >>> AverageEmbedding) ++
    Cosine(DocumentContext >>> BagOfWords >>> TopicModel)
```
