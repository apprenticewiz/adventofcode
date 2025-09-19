#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: filename [
    | stream input lines result codeLen encLen ch |
    [
      stream := FileStream open: filename mode: #read.
      input := stream contents asString.
      stream close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].

    lines := input lines.
    result := 0.
    lines do: [:line |
      codeLen := line size.
      encLen := 0.
      1 to: (line size) do: [:i |
        encLen := encLen + 1.
        ch := line at: i.
        (ch = $\) ifTrue: [ encLen := encLen + 1 ].
        (ch = $") ifTrue: [ encLen := encLen + 1 ]
      ].
      result := result + 2 + (encLen - codeLen)
    ].
    ^result
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
