Object subclass: Day01b [
    Day01b class >> usage [
       'usage: ./run.sh <progname> <file>' displayNl.
    ]

    Day01b class >> process: contents [
        | result digitsMap minIndex maxIndex leftDigit rightDigit leftIndex rightIndex |
        result := 0.
        digitsMap := LookupTable new.
        digitsMap at: '0' put: 0.
        digitsMap at: '1' put: 1.
        digitsMap at: '2' put: 2.
        digitsMap at: '3' put: 3.
        digitsMap at: '4' put: 4.
        digitsMap at: '5' put: 5.
        digitsMap at: '6' put: 6.
        digitsMap at: '7' put: 7.
        digitsMap at: '8' put: 8.
        digitsMap at: '9' put: 9.
        digitsMap at: 'zero' put: 0.
        digitsMap at: 'one' put: 1.
        digitsMap at: 'two' put: 2.
        digitsMap at: 'three' put: 3.
        digitsMap at: 'four' put: 4.
        digitsMap at: 'five' put: 5.
        digitsMap at: 'six' put: 6.
        digitsMap at: 'seven' put: 7.
        digitsMap at: 'eight' put: 8.
        digitsMap at: 'nine' put: 9.
        contents linesDo: [ :line | 
            minIndex := nil.
            maxIndex := nil.
            leftDigit := nil.
            rightDigit := nil.
            digitsMap keysDo: [ :digit |
                (line ~ digit) ifTrue: [
                    leftIndex := (line indexOfRegex: digit) first.
                    1 to: (line size) do: [ :i |
                        | tempVal |
                        tempVal := line indexOfRegex: digit startingAt: i.
                        tempVal ifNotNil: [ rightIndex := (tempVal first) ].
                    ].
                    (minIndex isNil or: [ leftIndex < minIndex ]) ifTrue: [
                        minIndex := leftIndex.
                        leftDigit := digitsMap at: digit.
                    ].
                    (maxIndex isNil or: [ rightIndex > maxIndex ]) ifTrue: [
                        maxIndex := rightIndex.
                        rightDigit := digitsMap at: digit.
                    ].
                ].
            ].
            result := result + ((leftDigit * 10) + rightDigit).
        ].
        ^ result.
    ]

    Day01b class >> main [
        | filename contents result |
        (Smalltalk arguments size < 1) ifTrue: [ ^self usage ].
        filename := Smalltalk arguments at: 1.
        contents := (FileDescriptor open: filename mode: #read) contents.
        result := self process: contents.
        ('result = ' , (result printString)) displayNl.
    ]
]

Day01b main.
