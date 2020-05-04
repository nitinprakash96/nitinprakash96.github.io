---
layout:     post
title:      Clojure. An introduction
author:     Nitin Prakash
summary:    Is learning clojure worth it for web developers?
category:   Blog
tags:       [clojure]
---

```
Whatever you background, Clojure is almost guaranteed to affect 
the way you think about programming.
- Eli Bendersky
```

I've been meaning to start a series on Clojure for quite a while now. I started exploring functional programming around June, 2019 and stumbled upon this language. I sure had my doubts on how impactful it might be. However, I was quite shocked with the pleasant development experience. The elegant integration with emacs and the joy of using REPL are all something that keeps me enganged in exploring clojure more and more everyday. I can keep going but first, let's dig into the background of Clojure so that we don't get lost right at the beginning.

```
A language that doesn't affect the way you think about programming,
is not worth knowing.

- Alan Perlis
```

### What is clojure?

* A functional programming language.
* A complied language. Yet dynamic in nature i.e., clojure features are also supported at runtime.
* Values built-in data structure philosphy. That is, it provides built-in support for _map_, _vector_, _hash-map_, _list_ etc.
* Provides persitent data structures that essentially focuses on immutability and multi-threading.
* Shares _code as data_ philosphy with a macro system.

For those of you who know, it's a LISP! Don't get confused now. Being a lisp does not mean it can't be used to build production ready softwares. It's fairly important to understand that clojure comes with a lot of features alongside being a lisp.

<p align="center">
  <img src="/images/6_post/clojure_intro.jpg">
</p>

Having said that, clojure provides a very powerful support for interfaces. These interfaces are called _protocols_ and discourages thinking in terms of classes with inheritance. It supports _multiple dispatch_ functions as well. Clojure also provides a great support for concurrency and parallelism in multiple ways.


### Code as data

Let's consider the follwing block of code. By the looks of it, anyone can say that it takes two arguments, _x_ and _y_ and returns the addition of the two.

```clojure
(defn add
  [x y]
  (+ x y))
```

Hold your horses. Let's take a close look and try to understand the way it can be interpreted. In the above code,

* _x_ and _y_ are __symbols__. A symbol is basically an object representing a name. These are first class names in Clojure.
* [x y] is a vector containing the symbols.
* (+ x y) is a linked list data structure containing two symbols _x_ and _y_.

From the above, I want to infer that when we talk about any piece of clojure code,
* We talk about a code that will execute.
* We think of it as a data structure consisting of _vectors_, _maps_, _intergers_, _strings_ etc.

In computer programming, this is called [Homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity). Clojure is said to be homoiconic. In simple terms, it is said to share _code as data_ philosphy. Code forms are data structures and data structures can be thought of as forms and executed as code.

<p align="center">
  <img src="/images/6_post/code_as_data.jpg" style="width:350px;height:300px;">
</p>

We'll look at some basic terms before diving deep into web programming.

### Functions

Creating and using functions, and creating functions that use functions, is what Clojure programers do. Clojure has a number of ways to create a function. I'll give an overview on some of them.

```clojure
(def print-string 
  (fn [] "That's cute. What else clojure is capable of?"))
=> #'user/print-string

(print-string)
=> "That's cute. What else clojure is capable of?"
```

_def_ is used declare a variable. _fn_ creates a simple function object. It can be stored in a var, passed to functions etc.
_defn_ is a macro that makes defining functions a little bit simpler. The above can be written as:

```clojure
user> (defn print-string
        []
        "That's cute. What else clojure is capable of?")
=> #'user/print-string

user> (print-string)
=> "That's cute. What else clojure is capable of?"
```

We can also overload arity in a single function, self-reference it.

```clojure
user> (defn sum
        ([x] x)
        ([x y] (+ x y))
        ([x y z] (+ x y z)))
=> #'user/sum

user> (sum 1)
1
user> (sum 1 2)
3
user> (sum 1 2 4)
7
```


### REPL

This stands for _Read-Eval-Print-Loop_. The REPL reads in text through the reader and further transforms it into a Clojure data structure. We can imagince REPL as our playground where we can test running code or even try out new ideas.

The reason it is such a fantastic feature is because of the feedback loop which tremendously enhances development cycle. This kind of feature is not available for most languages. We'll see the power of REPL in further sections. 


### Pure functions

These are functions that will always produce the same result for a given input. These cannot have any observable side effects and don't depend on any kind of outside state, other than that which was given as arguments to the function. The result of such functions don't change during the execution of the program or between executions of the program, as the dependency on outside state can lead to changes in the result of the function.


> Side effects are observed application state change outside a function

```clojure
user> (defn product
        [x y] (* x y))
=> #'user/sum

user> (product 1 2)
2
```

The above function _product_ will always result the same for a fixed _x_ and _y_. Even the examples above where we discussed multi arity were pure functions.

But,

```clojure

user> (def principal 1000)
=> #'user/amount

user> (defn interest
        [time rate] (/ (* principal time rate) 100))
=> #'user/interest

user> (interest 10 5)
=> 50000
```

The above function _interest_ is not a pure function as it depends on a variable _principal_. If it changed, _interest_ would produce a different result for the same _time_ and _rate_. These are called __impure functions__.


### Keywords

According to Clojure documentation, _keywords_ can be described as symbols that evaluate to themeselves.

```clojure
user> :foobar
=> :foobar

user> (keyword? :foorbar)
=> true

user> (keyword 100)
=> nil
```

Don't worry about the functioning of `keyword?` right now. We'll get to inbuilt functions soon. What I would like you to understand is that keywords provide a very fast equality test. And also these are the keys of a hashmap in clojure.

```clojure
user> (def sample-map {:a 1 :b 2})
=> #'user/sample-map

user> sample-map
=> {:a 1, :b 2}

user> (:a sample-map)
=> 1
```

### Collections and Sequences

There a lot of operations over collections and sequences provided by clojure. But before diving into those operation, we need to understand the difference between a _sequence_ abstraction and a _collection_ abstraction.

> Every sequence is a collection, but not every collection is a sequence.

<p align="center">
  <img src="/images/6_post/seq_vs_coll.jpg" style="width:350px;height:300px;">
</p>

The collection abstraction is closely related to the sequence abstraction. All of Clojure's core data structures — vectors, maps, lists and sets — take part in both abstractions. The main difference is that _sequence_ is about operating on the _individual element_ that is expected to be accessed in linear fashion whereas _collection_ is more about the overall data structure.

```clojure
user.core> (def foo '[1 2 3 4])
=> #'user/foo
user.core> foo
=> [1 2 3 4]

;; An individual element can't be empty. 
;; Only the whole data structure can be emtpty. 
;; Therefore, when we talk about collections we 
;; are more so referring to the data structure as whole
user.core> (empty? foo)
=> false
```

[This stackoverflow answer](https://stackoverflow.com/a/22439707/6244324) can be helpful in getting the overview easily.


### Starting a simple project

This section focuses on generating an outline for a simple clojure project (not for web programming). We will use [Leiningen](https://leiningen.org/) as our tool to automate any sort of clojure project throughout the series. There are others such as _boot_ but we will focus on _lein_.

To generate a project skeleton, a simple _lein_ command can do the trick:
```
lein new app clojure-first
```

This should genetate a structure as follows:

```
├── CHANGELOG.md
├── LICENSE
├── README.md
├── doc
│   └── intro.md
├── project.clj
├── resources
├── src
│   └── clojure_first
│       └── core.clj
├── target
└── test
    └── clojure_first
        └── core_test.clj
```

To run the project,

```
lein run
```

This command successfully runs because there is a `-main` function in `src/clojure_first/core.clj` which is the entry point for the app. Remove that and you might end up setting your hair on fire. Well, to ensure that doesn't happend we depend on lein.

To try out repl, you can run

```
lein repl
```

### Conclusion

In this post we figured out:

1. What is clojure?
2. Clojure philosphy.
3. Basic terms related to clojure.
4. Starting a simple clojure project.

I'm not going to debate on what editor you should and should not use. But I highly recommend one of the follwing:
1. Emacs + cider
2. IntelliJ + Cursive

Both of the above are the most used development environment for clojure. I personally use spacemacs (an emacs flavour) for clojure development.

In the next post we'll setup a web project and try to understand it's working.