#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> prop1: str [
    | vowels |
    vowels := 0.
    str do: [:ch |
      ('aeiou' includes: ch) ifTrue: [ vowels := vowels + 1 ]
    ].
    ^(vowels >= 3)
  ]

  Program class >> prop2: str [
    | hasRepeat i |
    hasRepeat := false.
    1 to: (str size - 1) do: [ :i |
      ((str at: i) = (str at: i + 1)) ifTrue: [
        hasRepeat := true
      ]
    ].
    ^hasRepeat
  ]

  Program class >> prop3: str [
    ^((str indexOfSubCollection: 'ab' startingAt: 1) = 0)
        and: [ ((str indexOfSubCollection: 'cd' startingAt: 1) = 0)
          and: [ ((str indexOfSubCollection: 'pq' startingAt: 1) = 0) 
            and: [ (str indexOfSubCollection: 'xy' startingAt: 1) = 0 ] ] ]
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
      (self prop1: line) and: [ (self prop2: line) and: [ (self prop3: line) ifTrue: [ count := count + 1 ] ] ]
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
