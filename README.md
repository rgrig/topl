Tools for [Temporal Object-oriented Property Language](http://rgrig.github.com/topl).

To try it, use

    docker run -ti rgrig/topl

Then try

    make test
    ls examples
    ./toplc -help

If you want to have access to the current directory while in the docker image,
say

    docker run -v $PWD:$PWD -ti rgrig/topl

To setup a development environment, do what
  [Dockerfile](https://github.com/rgrig/topl/blob/master/Dockerfile) says.
Compile with `make`,
  set up the path with `source setenv`,
  then test with `make test`.
