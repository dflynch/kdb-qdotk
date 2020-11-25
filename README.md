# kdb+qdotk
The q programming language is an embedded domain-specific language, implemented in k. The purpose of the language is to bootstrap k for time-series analysis by including additional operators for working with tables. Much of this functionality is defined in the `q.k` file that resides in the `QHOME` directory. 

Although documentation of q is excellent, there is no public documentation of k so anyone looking to peak under the hood must do so without a flashlight. The purpose of this repository is to be a Rosetta Stone of sorts: symbolic k code written in the more readable, q.

The contents of `q.k` are organised into namespaces. Here, each namespace is given a separate q file.

Before diving in, it is worth reminding ourselves of the primary difference between k and q syntax: k is purely symbolic, while q is a mix of words and symbols. Each of the 20 symbols on the ASCII keyboard is a primitive verb. In k, meaning is inferred from context; verbs can be applied prefix or infix making them monadic or dyadic. In q, monadic use cases are replaced by words...
