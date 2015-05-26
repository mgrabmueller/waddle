# waddle

## DOOM WAD file utilities

Waddle is a library for reading the contents of WAD files, as used by
the DOOM engine.

It can be used to parse WAD files into Haskell data
types. Additionaly, WAD data can be exported to JavaScript code.

In the folder "visualizer", there is a HTML5/Canvas viewer for the
exported data.

Waddle has been tested with DOOM.WAD, DOOM2.WAD, PLUTONIA.WAD and
TNT.WAD from Ultimate DOOM, DOOM II and Final DOOM.

To try it out, make sure you have a WAD file available and do the
following:

    tar xzvf waddle-X.Y.Z.W.tar.gz
    cd waddle-X.Y.Z.W
    cabal sandbox init
    cabal install
    mkdir visualize/data
    cabal run DOOM.WAD visualize/data

Then fire up a capable web browser and open visualize/waddle.html.

Happy WAD hacking!
  Martin
