#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: filename [
    | stream input lines totalLen parts l w h perim1 perim2 perim3 presentLen bowLen |
    [
      stream := FileStream open: filename mode: #read.
      input := stream contents asString.
      stream close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].

    lines := input lines.
    totalLen := 0.
    lines do: [:line |
      parts := line tokenize: 'x'.
      l := (parts at: 1) asInteger.
      w := (parts at: 2) asInteger.
      h := (parts at: 3) asInteger.
      perim1 := 2 * (l + w).
      perim2 := 2 * (l + h).
      perim3 := 2 * (w + h).
      presentLen := (perim1 min: perim2) min: perim3.
      bowLen := l * w * h.
      totalLen := totalLen + presentLen + bowLen.
    ].

    ^totalLen
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
