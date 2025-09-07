#!/usr/bin/env -S gst -f

Object subclass: Operation [
  | operator source1 source2 amount |

  Operation class >> operator: op source1: src1 [
    ^self new setOperator: op setSource1: src1.
  ]

  Operation class >> operator: op source1: src1 source2: src2 [
    ^self new setOperator: op setSource1: src1 setSource2: src2.
  ]

  Operation class >> operator: op source1: src1 amount: amt [
    ^ self new setOperator: op setSource1: src1 setAmount: amt.
  ]

  amount [
    ^amount
  ]

  operator [
    ^operator
  ]

  source1 [
    ^source1
  ]

  source2 [
    ^source2
  ]

  setOperator: op setSource1: src1 [
    operator := op.
    source1 := src1
  ]

  setOperator: op setSource1: src1 setSource2: src2 [
    operator := op.
    source1 := src1.
    source2 := src2
  ]

  setOperator: op setSource1: src1 setAmount: amt [
    operator := op.
    source1 := src1.
    amount := amt
  ]
].

Object subclass: Program [
  Program class >> usage [
    Transcript show: 'usage: program.st <input file>'; cr.
    ObjectMemory quit: 1.
  ]

  Program class >> process: filename [
    | file operations cache src1 src2 dest amt opStr op a |
    operations := Dictionary new.
    cache := Dictionary new.
    [
      file := (File name: filename) readStream.
      file linesDo: [:line |
        (line =~ '^(\d+|\w+) -> (\w+)$') ifMatched: [:match |
          src1 := match at: 1.
          dest := match at: 2.
          op := Operation operator: 'ASSIGN' source1: src1.
          operations at: dest put: op.
        ].
        (line =~ 'NOT (\d+|\w+) -> (\w+)') ifMatched: [:match |
          src1 := match at: 1.
          dest := match at: 2.
          op := Operation operator: 'NOT' source1: src1.
          operations at: dest put: op.
        ].
        (line =~ '(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)') ifMatched: [:match |
          src1 := match at: 1.
          opStr := match at: 2.
          src2 := match at: 3.
          dest := match at: 4.
          op := Operation operator: opStr source1: src1 source2: src2.
          operations at: dest put: op.
        ].
        (line =~ '(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)') ifMatched: [:match |
          src1 := match at: 1.
          opStr := match at: 2.
          amt := (match at: 3) asInteger.
          dest := match at: 4.
          op := Operation operator: opStr source1: src1 amount: amt.
          operations at: dest put: op.
        ].
      ].
      file close.
    ] on: SystemExceptions.FileError do: [:ex |
      Transcript show: 'Error reading file: ', ex messageText; cr.
      ObjectMemory quit: 1.
    ].
    a := self eval: 'a' withOperations: operations andCache: cache.
    op := Operation operator: 'ASSIGN' source1: (a printString).
    operations at: 'b' put: op.
    cache := Dictionary new.
    ^self eval: 'a' withOperations: operations andCache: cache
  ]

  Program class >> eval: expr withOperations: ops andCache: cache [
    | op a b r masked |
    ((expr =~ '\d+') matched) ifTrue: [
      ^(expr asInteger)
    ].
    (cache includesKey: expr) ifTrue: [
      ^(cache at: expr)
    ].
    op := ops at: expr.
    ((op operator) = 'ASSIGN') ifTrue: [
      a := self eval: (op source1) withOperations: ops andCache: cache.
      r := a
    ].
    ((op operator) = 'NOT') ifTrue: [
      a := self eval: (op source1) withOperations: ops andCache: cache.
      r := a bitInvert
    ].
    ((op operator) = 'AND') ifTrue: [
      a := self eval: (op source1) withOperations: ops andCache: cache.
      b := self eval: (op source2) withOperations: ops andCache: cache.
      r := a bitAnd: b
    ].
    ((op operator) = 'OR') ifTrue: [
      a := self eval: (op source1) withOperations: ops andCache: cache.
      b := self eval: (op source2) withOperations: ops andCache: cache.
      r := a bitOr: b
    ].
    ((op operator) = 'LSHIFT') ifTrue: [
      a := self eval: (op source1) withOperations: ops andCache: cache.
      r := a bitShift: (op amount)
    ].
    ((op operator) = 'RSHIFT') ifTrue: [
      a := self eval: (op source1) withOperations: ops andCache: cache.
      r := a bitShift: ((op amount) negated)
    ].
    masked := r bitAnd: 65535.
    cache at: expr put: masked.
    ^masked
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
