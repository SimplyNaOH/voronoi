# voronoi
A haskell implementation of Fortune's Algorithm. Bear in mind: this is a work in progress!


## tree-render branch
This branch is for debugging/demonstration purposes. It renders the Breakpoint
Binary Search Tree as it changes during the algorithm.

## Usage

    mkdir anim
    stack exec voronoi-exe
This will ask you for the number of centers to run the algorithm on, the number
of successive steps to render the tree, and the initial step. If the number of
steps is -1, it will run until the end.

An animation can be made out of the result for example using ffmpeg:

    ffmpeg -r 4 -i ./anim/out_%04d.jpg -pix_fmt yuv420p ./anim/out.mp4
Where `-r 4` sets the frames per second to 4, and `-pix_fmt yuv420p` is used for compatibility issues.

## Output
The output images are a schematic representation of the binary tree. Each node
in the image represents a Breakpoint. Events are color coded:

1. When the next event is a CircleEvent, **yellow** is used to highlight the
Breakpoints about to be merged.
2. If the event just processed was a NewPoint event, **red** is used to highlight
the new pair of Breakpoints.
3. If the event just processed was a CircleEvent, **blue** is used to highlight
the resulting new Breakpoint.

There is an example included, example.gif:

<img src=https://github.com/SimplyNaOH/voronoi/blob/tree-render/example.gif>.
