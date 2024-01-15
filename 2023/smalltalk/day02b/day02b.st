Object subclass: Day02b [
    Day02b class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl.
    ]

    Day02b class >> process: contents [
        | result gameNum restPart color amount redNeeded greenNeeded blueNeeded |
        result := 0.
        contents linesDo: [ :line | 
            gameNum := ((((line tokenize: ': ') first) tokenize: ' ') second) asInteger.
            restPart := (line tokenize: ': ') second.
            redNeeded := 0.
            greenNeeded := 0.
            blueNeeded := 0.
            (restPart tokenize: '; ') do: [ :draws |
                (draws tokenize: ', ') do: [ :draw |
                    color := (draw tokenize: ' ') second.
                    amount := ((draw tokenize: ' ') first) asInteger.
                    ((color = 'red') & (amount > redNeeded)) ifTrue: [ redNeeded := amount ].
                    ((color = 'green') & (amount > greenNeeded)) ifTrue: [ greenNeeded := amount ].
                    ((color = 'blue') & (amount > blueNeeded)) ifTrue: [ blueNeeded := amount ].
                ].
            ].
            result := result + (redNeeded * greenNeeded * blueNeeded ).
        ].
        ^ result.
    ]

    Day02b class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl.
    ]
]

Day02b main.
