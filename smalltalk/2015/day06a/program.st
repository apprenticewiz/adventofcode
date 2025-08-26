#!/usr/bin/env -S gst -f

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> perform: action on: grid fromUpperLeftRow: r1 andUpperLeftCol: c1 toLowerRightRow: r2 andLowerRightCol: c2 [
    | oldVal index |
    r1 to: r2 do: [:row |
      c1 to: c2 do: [:col |
        index := ((row - 1) * 1000) + col.
        (action = 'turn on') ifTrue: [grid at: index put: true].
        (action = 'turn off') ifTrue: [grid at: index put: false].
        (action = 'toggle') ifTrue: [
            oldVal := grid at: index.
            grid at: index put: (oldVal not)
        ]
      ]
    ]
  ]

  Program class >> sum: grid [
    | total |
    total := 0.
    1 to: (1000 * 1000) do: [:i |
      (grid at: i) ifTrue: [ total := total + 1 ]
    ].
    ^total
  ]

  Program class >> process: filename [
    | grid file action r1 c1 r2 c2 |
    grid := Array new: (1000 * 1000).
    1 to: (1000 * 1000) do: [:i | grid at: i put: false].
    [
      file := (File name: filename) readStream.
      file linesDo: [:line |
          (line =~ '(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)') ifMatched: [:match |
              action := match at: 1.
              r1 := ((match at: 2) asInteger) + 1.
              c1 := ((match at: 3) asInteger) + 1.
              r2 := ((match at: 4) asInteger) + 1.
              c2 := ((match at: 5) asInteger) + 1.
              self perform: action on: grid fromUpperLeftRow: r1 andUpperLeftCol: c1 toLowerRightRow: r2 andLowerRightCol: c2
          ]
      ].
      file close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].
    ^self sum: grid
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
