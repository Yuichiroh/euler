Project Euler in Scala
===

How to run codes
---

git clone https://github.com/Yuichiroh/euler.git

cd euler

mkdir bin

scalac -d bin src/main/scala/euler/*.scala

scala -J-Dfile.encoding=UTF-8 -J-Xmx1g -Dscala.time -cp .:bin:data euler.P1
