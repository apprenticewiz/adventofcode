
Object subclass: Day04a [
    Day04a class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl
    ]

    Day04a class >> process: contents [
        | winningStr handStr winningSet handSet intersection result |
        result := 0.
        contents linesDo: [ :line |
            winningStr := (((line tokenize: ':\s+') second) tokenize: '\s+\|\s+') first.
            winningSet := Set new.
            (winningStr tokenize: '\s+') do: [ :numStr |
                winningSet add: (numStr asInteger)
            ].
            handStr := (((line tokenize: ':\s+') second) tokenize: '\s+\|\s+') second.
            handSet := Set new.
            (handStr tokenize: '\s+') do: [ :numStr |
                handSet add: (numStr asInteger)
            ].
            intersection := winningSet & handSet.
            ((intersection size) > 0) ifTrue: [
                result := result + (1 bitShift: ((intersection size) - 1))
            ]
        ].
        ^ result
    ]

    Day04a class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl
    ]
]

Day04a main.
