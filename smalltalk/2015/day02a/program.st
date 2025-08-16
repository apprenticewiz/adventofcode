#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: filename [
    | stream input lines totalArea parts l w h area1 area2 area3 surfaceArea minArea |
    [
      stream := FileStream open: filename mode: #read.
      input := stream contents asString.
      stream close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].

    lines := input lines.
    totalArea := 0.
    lines do: [:line |
      parts := line tokenize: 'x'.
      l := (parts at: 1) asInteger.
      w := (parts at: 2) asInteger.
      h := (parts at: 3) asInteger.
      area1 := l * w.
      area2 := l * h.
      area3 := w * h.
      surfaceArea := (2 * area1) + (2 * area2) + (2 * area3).
      minArea := (area1 min: area2) min: area3.
      totalArea := totalArea + surfaceArea + minArea.
    ].

    ^totalArea
  ]

  Program class >> main: args [
    | filename result |
    (args size < 1) ifTrue: [ ^self usage ].
    filename := args first.
    result := self process: filename.
    Transcript show: 'result = '; show: result printString; cr.
  ]
].

Program main: Smalltalk arguments.
