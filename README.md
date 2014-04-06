Project Euler in Scala
===

How to run codes
---
scalac -d bin euler/scala/*.scala

scala -J-Dfile.encoding=UTF-8 -J-Xmx1g -Dscala.time -cp bin:.:euler/data euler.scala.P1