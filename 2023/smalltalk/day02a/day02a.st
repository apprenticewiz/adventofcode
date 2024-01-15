Object subclass: Day02a [
    Day02a class >> totalRed [ ^ 12. ]

    Day02a class >> totalGreen [ ^ 13. ]

    Day02a class >> totalBlue [ ^ 14. ]

    Day02a class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl.
    ]

    Day02a class >> process: contents [
        | result gameNum restPart color amount valid |
        result := 0.
        contents linesDo: [ :line | 
            gameNum := ((((line tokenize: ': ') first) tokenize: ' ') second) asInteger.
            restPart := (line tokenize: ': ') second.
            valid := true.
            (restPart tokenize: '; ') do: [ :draws |
                (draws tokenize: ', ') do: [ :draw |
                    color := (draw tokenize: ' ') second.
                    amount := ((draw tokenize: ' ') first) asInteger.
                    ((color = 'red') & (amount > (Day02a totalRed))) ifTrue: [ valid := false ].
                    ((color = 'green') & (amount > (Day02a totalGreen))) ifTrue: [ valid := false ].
                    ((color = 'blue') & (amount > (Day02a totalBlue))) ifTrue: [ valid := false ].
                ].
            ].
            valid ifTrue: [ result := result + gameNum ].
        ].
        ^ result.
    ]

    Day02a class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl.
    ]
]

Day02a main.
