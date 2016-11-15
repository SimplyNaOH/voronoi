# voronoi
A haskell implementation of Fortune's Algorithm. Bear in mind: this is a work in progress!


## Usage

    stack exec voronoi-exe -- -o [filename].svg -w [width]
You can also try `stack exec voronoi-exe -- -h` for more options.

## Description

The library code is in voronoi/src/Fortune.hs, where the Module **Fortune** is defined. This module exports the function **voronoi** and the type **Edge'**.

The type **Edge'** is a single constructor type with two indices (**Int**s) and two points ( **(Double, Double)** ). *Edge i j a b* represents an edge between the *i*th and *j*th voronoi cells, that goes from point *a* to *b*.


The function **voronoi** implements Fortune's Algorithm to generate the voronoi diagram corresponding to a set of points.

The executable with source in voronoi/app/Main.hs uses the Diagrams framework to output an svg of the voronoi diagram of a random set of points. As an example, you can view test.svg:

<img src=https://cdn.rawgit.com/SimplyNaOH/voronoi/master/test.svg>.

## TODO:

* ~~Implement Fortune's Algorithm.~~
* ~~Solve space leak in current implementation.~~
* ~~Use a more appropiate data-structure for the lists used in the algorithm.~~
* Improve performance.
* Calculate Delaunay triangulation from the voronoi diagram.
* Implement Lloyd's algorithm.
