## Probe
Discriminative feature-based information retrieval and an NLP feature engineering framework

Current version: **`0.9.0`**

If you are using it in an academic work, please cite
```tex
@inproceedings{chen2017discriminative,
  title={Discriminative information retrieval for question answering sentence selection},
  author={Chen, Tongfei and Van Durme, Benjamin},
  booktitle={Proceedings of EACL},
  year={2017}
}
```

`Probe` contains two modules:
 - [`probe-core`](https://github.com/ctongfei/probe/tree/master/core): compositional, declarative framework for feature engineering
 - [`probe-search`](https://github.com/ctongfei/probe/tree/master/search): Implements feature-based discriminative IR based on [Apache Lucene](http://lucene.apache.org/) and `probe-core`.
 
 Guide to each can be found in their respective readme files.
 
 #### Changelog
 - `0.9.0`: Supports weighted indices through Lucene's payload mechanism.
 - `0.8.0`: Merged `probe-search` from a standalone package.
 - `0.7.4`: Added `Featurizer.optional`.
 - `0.7.3`: Maintenance version bump for depending libraries.
 - `0.7.2`: Bugfix: escape space to underscore in feature vector formatters.
 - `0.7.1`: Various dot operations on feature families.
 - `0.7.0`: Feature families.