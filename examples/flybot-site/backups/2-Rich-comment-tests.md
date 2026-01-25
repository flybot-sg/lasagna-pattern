---
title: Rich comment tests
id: 2
author: lt@basecity.com
tags:
  - Blog
created-at: "2023-03-16T00:00:00.000Z"
updated-at: "2023-03-16T00:00:00.000Z"
---

We Clojure programmers have REPL as our super weapon, doing unit tests are so easy and so fun by simply using clojure.test. However, Clojure tests are still not very convenient sometimes:

- It requires you put your tests in another namespace (you may put them in the source namespace, but it pollutes your production code, so almost no one use it this way), have to update two namespaces simutanously is painful.
- We always do scratch tests in our `comment`, just under a function's implementation. Watching it running in REPL, make sure it works as expected. What if this can turn into tests?

[Rich comment tests](https://github.com/matthewdowney/rich-comment-tests) is not the first tool trying to turn your comments into tests, but it is the most intuitive one.

- It does not pollute. Your tests just sitting inside old common comments. Only active if you are running tests.
- It follows most programmers' habit of defining results: just after your code, in a single-line comment style. Like this: `(+ 1 2) ;=> 3`, it treats these comments as your expected values, no need to explain.
- It has additional great feature of using macho pattern to specific a function to judge if results of code as expected by adding an arrow. e.g. `(+ 1 1) ;=>> even?`

I immediately fell in love with this little tool, however, it lacks some features I need (like specify an exception to be thrown), and contains some small bugs, I forked it, implemented/fixed them, but the PR is not accepted by the original auther yet. Enjoy now.
