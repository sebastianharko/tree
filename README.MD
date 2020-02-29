Minimum Path Problem
====================

How to run
==========

To run unit tests 
(NOTE: there is a unit test that makes sure that the solution works for a 500-row triangle found in test/resources/data.txt)
```
sbt test
```

To run CLI version:

```
sebastians-MacBook-Pro:tree sebastian$ cat << EOF | sbt run
> 1
> 1 2
> 1 2 3
> EOF
[info] Loading global plugins from /Users/sebastian/.sbt/1.0/plugins
[info] Loading settings for project tree-build from plugins.sbt ...
[info] Loading project definition from /Users/sebastian/ncl/silly/Triangle/tree/project
[info] Loading settings for project tree from build.sbt ...
[info] Set current project to Triangle (in build file:/Users/sebastian/ncl/silly/Triangle/tree/)
[info] Compiling 1 Scala source to /Users/sebastian/ncl/silly/Triangle/tree/target/scala-2.12/classes ...
[info] Done compiling.
[info] running com.harko.Main
Minimal path is: 1 + 1 + 1 = 3
[success] Total time: 5 s, completed 28-Feb-2020 4:09:05 PM
```