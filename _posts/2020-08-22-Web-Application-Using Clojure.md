---
layout:     post
title:      Web application using Clojure
author:     Nitin Prakash
summary:    Let clojure be your magic wand.
category:   Blog
tags:       [clojure]
---

Clojure is an incredibly powerful language. But true power of a language is unleashed through it's _extensibilty_. This is true for most of programming languages. The capability of an application to handle new modules and expand functionalities is what determines the life of a language as a production system tool.

I keep saying _production ready_ or _production system_, but what does it really mean? By throwing those words around I really want to refer to a system that
- runs
- stable, scalable and maintainable
- has well though out design
- documented (Underrated but very important)

In this post, I'll be mostly discussing how to build a web application using Clojure as a language. Also we'll look at some functional programming concepts along the way.
And our web server will be backed by __Pedestal__ which is a framework for writing web services. The goal for this chapter is to have a simple server running by the end.

### Pedestal

As per the documentation, Pedestal is a set of libraries that we use to build services and applications. It runs in the back end and can serve up whole HTML pages or handle API requests.
Primarily, usage of pedestal means that we build APIs first. It lets us start simple and build on top of that as we proceed.

There are several reasons for chosing Pedestal for writing web services. Some of them are:
- Interceptors (building blocks of Pedestal) let services apply different behavior based on each incoming request, rather than statically wrapping everything.
- Various methods of deplyoment at our disposal (Eg: JAR, containers, unikernel etc).
- Creating dynamic application is easy with web sockets.

