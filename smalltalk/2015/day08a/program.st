#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: filename [
    | stream input lines result codeLen memLen i ch1 ch2 |
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
      memLen := 0.
      i := 2.
      [ i <= ((line size) - 1) ] whileTrue: [
        ch1 := line at: i.
        ch2 := line at: (i + 1).
        ( ch1 = $\ ) ifTrue: [
          i := i + 1.
          ( ch2 = $\ ) ifTrue: [ i := i + 1 ].
          ( ch2 = $" ) ifTrue: [ i := i + 1 ].
          ( ch2 = $x ) ifTrue: [ i := i + 3 ]
        ].
        ( ch1 = $\ ) ifFalse: [ i := i + 1 ].
        memLen := memLen + 1
      ].
      result := result + (codeLen - memLen)
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
