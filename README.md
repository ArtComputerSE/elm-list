# elm-list
A list that the user can reorder by drag and drop. Works both with mouse and touch.

## Install

``install.sh`` Will install the Elm and Node packages used. The latter is for
running a local server only.

## Start

``run.sh`` Will start an Express server at [localhost:3005](http://localhost:3005)


## Credits
This was based on code from the repo [wintvelt/elm-sortable-list](https://github.com/wintvelt/elm-sortable-list) 
maintained by Wintvelt.

Uses the package [mpizenberg/elm-pointer-events](http://package.elm-lang.org/packages/mpizenberg/elm-pointer-events).

Uses [elm-pep](mpizenberg/elm-pep) a polyfill to compensate for recent browsers
that not yet supports pointer events.