## Probe
An NLP feature engineering framework for Scala

#### Introduction
In natural language processing, tasks that require heavy feature engineering is often encountered. 
These code often contains a lot of boilerplate and ad-hoc tricks, making the code hard to maintain.

`Probe` provides a principled, functional way for doing feature extraction by heavy use of compositional 
operators that leads to an elegant embedded DSL for feature extraction.

##### Installation

```scala
resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "me.tongfei" %% "probe" % "0.5.0-SNAPSHOT"
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
The `Feature` type in `Probe` is designed to capture all these examples listed above. 
A `Feature` consists of the following parts:
```scala
    "name"       "key"        "value"
    tfIdf   ~   avocado   :   7.4561
```
The name must be of type `String`, the value `Double`, and the key can be of any type `A`.

A `FeatureGroup` is a group of features that share the same `name`. The examples [3], [4]
and [5] are all `FeatureGroup`s.

A basic `FeatureGroup` trait is defined as follows:
```scala
trait FeatureGroup[A] {
  def name: String
  def pairs: Iterable[(A, Double)]
```


#### Featurizers and their compositional operators

#### Feature similarity measures

#### Featurizer set and the DSL
