# voronoi
A haskell implementation of Fortune's Algorithm. Bear in mind: this is a work in progress!


## Description

The library code is in voronoi/src/Fortune.hs, where the Module **Fortune** is defined. This module exports two types and the function **voronoi**.

The type **Point a** is just `type Point a = (a, a)`.

The type **Edge a** is defined as `data Edge a = Edge Index Index (Point a) (Point a)` where **Index** is just an alias of **Int**. The edge defined by `Edge i j l r` lies between the *i*th and *j*th centers and has vertices *l* and *r*, with **_i_ always less than _j_**.

The function **voronoi** implements Fortune's Algorithm to generate the voronoi diagram corresponding to a set of **Point**s. The type signature of voronoi, `voronoi :: (Floating a, Ord a) => [Point a] -> [Edge a]`, restricts a.

The executable with source in voronoi/app/Main.hs uses the Diagrams framework to output an svg of the voronoi diagram of a random set of points. As an example, you can view test.svg:

<img src=https://rawgit.com/SimplyNaOH/voronoi/master/test.svg>.

## TODO:

* ~~Implement Fortune's Algorithm~~
* Calculate Delaunay triangulation from the voronoi diagram
* Implement Lloyd's algorithm
