---
layout:     post
title:      Dynamic Programming
author:     Nitin Prakash
summary:    You can run, but you can't hide.
category:   Blog
---

Dynamic programming is a technique for solving a complex problem by breaking it down into a collection of simpler subproblems. Smaller subproblems are solved only once and stored in a data structure like array, map etc. The fundamental idea being to optimize a given recursive problem and improve upon the complexity.

Stating the same as an RL paradigm:

```
Dymanic programming refers to solving a given problem using a collection
of algorithms that computes optimal policies given a perfect model of the
environment as a finite Markov decision process.
```

From the above reference, it should be clear that when we talk about DP we are just formulating a structure for the search of an optimal policy. In any paradigm of solving problems, DP requires two conditions to be satisfied:
 - Optimal substructure
 - Ovarlapping subproblems

Fortunately, MDPs satisfy both the properties.