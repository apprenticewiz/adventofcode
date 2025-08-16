#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> prop1: str [
    | hasRepeat i pair |
    hasRepeat := false.
    1 to: (str size - 3) do: [ :i |
      pair := str copyFrom: i to: (i + 1).
      ((str indexOfSubCollection: pair startingAt: (i + 2)) ~= 0) ifTrue: [ hasRepeat := true ]
    ].
    ^hasRepeat
  ]

  Program class >> prop2: str [
    | hasRepeat i |
    hasRepeat := false.
    1 to: (str size - 2) do: [ :i |
      ((str at: i) = (str at: i + 2)) ifTrue: [
        hasRepeat := true
      ]
    ].
    ^hasRepeat
  ]

  Program class >> process: filename [
    | stream input lines count |
    [
      stream := FileStream open: filename mode: #read.
      input := stream contents asString.
      stream close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].

    lines := input lines.
    count := 0.
    lines do: [:line |
      (self prop1: line) and: [ (self prop2: line) ifTrue: [ count := count + 1 ] ]
    ].
    ^count
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
