### probe-search
Discriminative IR framework

`probe-search` solves the following ranking problem where *F* satisfies some constraints (please read the paper for full detail):
  
  <a href="https://www.codecogs.com/eqnedit.php?latex=\arg\max_{c&space;\in&space;C}&space;F(q,c)" target="_blank"><img src="https://latex.codecogs.com/gif.latex?\arg\max_{p&space;\in&space;P}&space;F(q,p)" title="\arg\max_{p \in P} F(q,p)" /></a>
  
where *q* is a query, *c* is a candidate, *C* is a candidate set and *F* is the scoring function.


#### Feature extractor
Define *F*, which is a feature extractor on a pair of (query, candidate) using `probe`'s DSL. An example for question-answering would be
```scala
    val F =
        (QuestionWordWithLexicalAnswerTypeFeature * NamedEntityTypesFeature) +
        (QuestionWordWithLexicalAnswerTypeFeature * BagOfWordsFeature) +
        (NamedEntityFeature =?= NamedEntityFeature) +
        (NormalizedTfIdfFeature =?= BagOfWordsFeature)
```

#### Model training
```scala
val model = Model.fitFrom(
  F, // Feature extractor on query/candidate pairs as the F defined above
  qcs, // question/candidate pairs
  ys,  // labels (0/1) indicating whether the corresponding question/candidate pair is relevant
  regCoefficient // regularization coefficient
)
model.saveToFile("path/to/save/the/model/to")
model = Model.loadFromFile("path/to/load/the/model/from") // to recover the model from a file
```
Ranking SVM will be released later.

#### Index building
```scala
val Fc = F.projectSecond // gets the feature extractor on candidates
val ib = IndexBuilder("path/to/store/the/index")
for ((id, doc) <- xs) {
  ib.write(id, Fc(doc)) // write the feature vector of the given candidate to the index with the associated id
}
ib.close()
```

#### Retrieval
```scala
val Fq = F.projectFirst // gets the feature extractor on queries
val projector = Projector.fromModel(model) 
// Builds a projector from the model. A projector has the property
// projector.project(Fq(q)) dot Fc(c) == F(q, c)
val engine = new QueryEngine("path/where/the/index/is/stored")
engine.query(
  projector.project(Fq(q)), // execute search
  k  // top-k candidates that are desired
)
```
