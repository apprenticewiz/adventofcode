
Object subclass: Day04b [
    Day04b class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl
    ]

    Day04b class >> process: contents [
        | cardNumber count winningStr handStr winningSet handSet intersection instances copies result |
        result := 0.
        instances := Dictionary new.
        contents linesDo: [ :line |
            cardNumber := ((((line tokenize: ':\s+') first) tokenize: '\s+') second) asInteger.
            winningStr := (((line tokenize: ':\s+') second) tokenize: '\s+\|\s+') first.
            winningSet := Set new.
            (winningStr tokenize: '\s+') do: [ :numStr | winningSet add: (numStr asInteger) ].
            handStr := (((line tokenize: ':\s+') second) tokenize: '\s+\|\s+') second.
            handSet := Set new.
            (handStr tokenize: '\s+') do: [ :numStr | handSet add: (numStr asInteger) ].
            intersection := winningSet & handSet.
            count := intersection size.
            (cardNumber + 1) to: (cardNumber + count) do: [ :i |
                copies := 0.
                (instances includesKey: i) ifTrue: [
                    copies := copies + (instances at: i)
                ].
                copies := copies + 1.
                (instances includesKey: cardNumber) ifTrue: [
                    copies := copies + (instances at: cardNumber)
                ].
                instances at: i put: copies
            ].
            result := result + 1
        ].
        (instances values) do: [ :i | result := result + i].
        ^ result
    ]

    Day04b class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl
    ]
]

Day04b main.
