Project Euler in Scala
===

Personal Rule
---
An execution time of each solution should be less than one second on my laptop machine (MacBook Air 11-inch, Mid 2013, 1.7GHz Intel Core i7, 8GB 1600 MHz DDR3).

This rule is currently broken on the following problems:

* Problem 60 - 9,472ms
* Problem 78 - 12,736ms
* Problem 86 - 2,125ms
* Problem 93 - 2,244ms
* Problem 95 - 3,854ms
* Problem 96 - 1,184ms


How to run codes
---

```
git clone https://github.com/Yuichiroh/euler.git
cd euler
sbt compile

scala -J-Dfile.encoding=UTF-8 -J-Xmx1g -Dscala.time -cp .:target/scala-2.11/classes:data yuima.euler.P1
```
