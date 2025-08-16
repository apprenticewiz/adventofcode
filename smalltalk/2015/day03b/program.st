#!/usr/bin/env -S gst -f

Object subclass: Position2D [
  | x y |

  Position2D class >> x: xVal y: yVal [
    ^self new setX: xVal y: yVal.
  ]

  setX: xVal y: yVal [
    x := xVal.
    y := yVal.
  ]

  setX: xVal [
    x := xVal.
  ]

  setY: yVal [
    y := yVal.
  ]

  x [ ^x ]

  y [ ^y ]

  asString [
    ^('Position2D(', (x asString), ', ', (y asString), ')')
  ]
].

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: filename [
    | stream input santa roboSanta santaMove positions |
    [
      stream := FileStream open: filename mode: #read.
      input := stream contents asString.
      stream close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].

    santa := Position2D x: 0 y: 0.
    roboSanta := Position2D x: 0 y: 0.
    positions := Set new.
    positions add: (santa asString).
    santaMove := true.
    input do: [:ch |
      santaMove ifTrue: [
        (ch = $^) ifTrue: [ santa setY: (santa y) + 1 ].
        (ch = $v) ifTrue: [ santa setY: (santa y) - 1 ].
        (ch = $<) ifTrue: [ santa setX: (santa x) - 1 ].
        (ch = $>) ifTrue: [ santa setX: (santa x) + 1 ].
        positions add: (santa asString).
      ].
      santaMove ifFalse: [
        (ch = $^) ifTrue: [ roboSanta setY: (roboSanta y) + 1 ].
        (ch = $v) ifTrue: [ roboSanta setY: (roboSanta y) - 1 ].
        (ch = $<) ifTrue: [ roboSanta setX: (roboSanta x) - 1 ].
        (ch = $>) ifTrue: [ roboSanta setX: (roboSanta x) + 1 ].
        positions add: (roboSanta asString).
      ].
      santaMove := santaMove not.
    ].

    ^(positions size).
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
