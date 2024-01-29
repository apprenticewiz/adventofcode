Object subclass: Position [
    | row col |

    Position class >> init [
        ^self error: 'use row:col:'
    ]

    Position class >> row: aRow col: aCol [
        ^(super new) setRow: aRow setCol: aCol
    ]

    Position >> setRow: aRow setCol: aCol [
        self setRow: aRow.
        self setCol: aCol.
        ^self
    ]

    Position >> setRow: aRow [
        row := aRow.
        ^self
    ]

    Position >> setCol: aCol [
        col := aCol.
        ^self
    ]

    Position >> row [
        ^row
    ]

    Position >> col [
        ^col
    ]
]

Object subclass: NumberLocation [
    | pos number |

    NumberLocation class >> init [
        ^self error: 'use pos:number:'
    ]

    NumberLocation class >> pos: aPos number: aNumber [
        ^(super new) setPos: aPos setNumber: aNumber
    ]

    NumberLocation >> setPos: aPos setNumber: aNumber [
        self setPos: aPos.
        self setNumber: aNumber.
        ^self
    ]

    NumberLocation >> setPos: aPos [
        pos := aPos.
        ^self
    ]

    NumberLocation >> setNumber: aNumber [
        number := aNumber.
        ^self
    ]

    NumberLocation >> pos [
        ^pos
    ]

    NumberLocation >> number [
        ^number
    ]
]

Object subclass: Day03b [
    Day03b class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl
    ]

    Day03b class >> buildNumbers: contents [
        | numberLocs row col scanningNumber number currentPos numberLoc |
        numberLocs := OrderedCollection new.
        row := 0.
        scanningNumber := false.
        number := String new.
        currentPos := Array new.
        contents linesDo: [ :line |
            col := 0.
            line do: [ :ch |
                scanningNumber
                    ifTrue: [
                        (ch isDigit)
                            ifTrue: [ number := number, (String with: ch) ]
                            ifFalse: [
                                numberLoc := NumberLocation pos: currentPos number: number.
                                numberLocs add: numberLoc.
                                scanningNumber := false.
                                number := String new
                            ]
                    ]
                    ifFalse: [
                        (ch isDigit)
                            ifTrue: [
                                currentPos := Position row: row col: col.
                                number := String with: ch.
                                scanningNumber := true
                            ]
                    ].
                col := col + 1
            ].
            scanningNumber ifTrue: [
                numberLoc := NumberLocation pos: currentPos number: number.
                numberLocs add: numberLoc.
                scanningNumber := false.
                number := String new
            ].
            row := row + 1
        ].
        ^ numberLocs
    ]

    Day03b class >> buildParts: contents [
        | partLocs row col |
        partLocs := OrderedCollection new.
        row := 0.
        contents linesDo: [ :line |
            col := 0.
            line do: [ :ch |
                (ch = $*) ifTrue: [ partLocs add: (Position row: row col: col) ].
                col := col + 1
            ].
            row := row + 1
        ].
        ^ partLocs
    ]

    Day03b class >> check: numberLocs with: partLocs [
        | result neighbors numberRow number numberColFirst numberColLast found adjPos adjCount prod |
        result := 0.
        neighbors := OrderedCollection new.
        neighbors add: (Position row: -1 col: -1).
        neighbors add: (Position row: -1 col: 0).
        neighbors add: (Position row: -1 col: 1).
        neighbors add: (Position row: 0 col: -1).
        neighbors add: (Position row: 0 col: 1).
        neighbors add: (Position row: 1 col: -1).
        neighbors add: (Position row: 1 col: 0).
        neighbors add: (Position row: 1 col: 1).
        partLocs do: [ :partLoc |
            adjCount := 0.
            prod := 1.
            numberLocs do: [ :numberLoc |
                numberRow := (numberLoc pos) row.
                number := numberLoc number.
                numberColFirst := (numberLoc pos) col.
                numberColLast := numberColFirst + (number size) - 1.
                found := false.
                (found not) ifTrue: [
                    neighbors do: [ :neighbor |
                        adjPos := Position row: ((partLoc row) + (neighbor row)) col: ((partLoc col) + (neighbor col)).
                        (found not) ifTrue: [
                            numberColFirst to: numberColLast do: [ :numberCol |
                                ((found not) & (numberRow = (adjPos row)) & (numberCol = (adjPos col))) ifTrue: [
                                    adjCount := adjCount + 1.
                                    prod := prod * (number asInteger).
                                    found := true
                                ]
                            ]
                        ]
                    ]
                ]
            ].
            (adjCount = 2) ifTrue: [ result := result + prod ]
        ].
        ^ result
    ]

    Day03b class >> process: contents [
        | numberLocs partLocs |
        numberLocs := self buildNumbers: contents.
        partLocs := self buildParts: contents.
        ^ (self check: numberLocs with: partLocs).
    ]

    Day03b class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl
    ]
]

Day03b main.
