
Object subclass: Day01a [
    Day01a class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl.
    ]

    Day01a class >> process: contents [
        | result digits minIndex maxIndex leftDigit rightDigit leftIndex rightIndex |
        result := 0.
        digits := (0 to: 9 by: 1) collect: [ :i | i printString ].
        contents linesDo: [ :line | 
            minIndex := nil.
            maxIndex := nil.
            leftDigit := nil.
            rightDigit := nil.
            digits do: [ :digit |
                (line ~ digit) ifTrue: [
                    leftIndex := (line indexOfRegex: digit) first.
                    1 to: (line size) do: [ :i |
                        | tempVal |
                        tempVal := line indexOfRegex: digit startingAt: i.
                        tempVal ifNotNil: [ rightIndex := (tempVal first) ].
                    ].
                    (minIndex isNil or: [ leftIndex < minIndex ]) ifTrue: [
                        minIndex := leftIndex.
                        leftDigit := digit.
                    ].
                    (maxIndex isNil or: [ rightIndex > maxIndex ]) ifTrue: [
                        maxIndex := rightIndex.
                        rightDigit := digit.
                    ].
                ].
            ].
            result := result + (leftDigit , rightDigit) asInteger.
        ].
        ^ result.
    ]

    Day01a class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl.
    ]
]

Day01a main.
