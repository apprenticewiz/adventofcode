open System
open System.IO

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let processInput (contents: string) =
    let instances =
        Seq.fold
            (fun (instances: Map<uint, uint>) (line: string) ->
                let cardPart = line.Split(": ")[0]
                let cardNumStr = (cardPart.Split (" ", StringSplitOptions.RemoveEmptyEntries))[1]
                let cardNumber = UInt32.Parse(cardNumStr)
                let rest = line.Split(": ")[1]
                let winningStr = rest.Split(" | ")[0]
                let winningSet =
                    Seq.fold
                        (fun set numStr ->
                            let num = UInt32.Parse(numStr)
                            Set.add num set)
                        Set.empty
                        (winningStr.Split (" ", StringSplitOptions.RemoveEmptyEntries))
                let handStr = rest.Split(" | ")[1]
                let handSet =
                    Seq.fold
                        (fun set numStr ->
                            let num = UInt32.Parse(numStr)
                            Set.add num set)
                        Set.empty
                        (handStr.Split (" ", StringSplitOptions.RemoveEmptyEntries))
                let intersection = Set.intersect winningSet handSet
                let count = uint (Set.count intersection)
                Seq.fold
                    (fun (prevInstances: Map<uint, uint>) (i: uint) ->
                        let copies =
                            (if (Map.containsKey i prevInstances) then (Map.find i prevInstances) else 0u) +
                            1u + (if (Map.containsKey cardNumber prevInstances) then (Map.find cardNumber prevInstances) else 0u)
                        Map.add i copies prevInstances
                    ) instances (Seq.toList (seq { (cardNumber + 1u)..(cardNumber + count) }))
            )
            Map.empty
            (contents.Split ('\n', StringSplitOptions.RemoveEmptyEntries))
    (uint (Seq.length (contents.Split ('\n', StringSplitOptions.RemoveEmptyEntries)))) + (Seq.sum (Map.values instances))

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        usage()
    let filename = argv.[0]
    let contents = File.ReadAllText(filename)
    let result = processInput contents
    printfn "result = %d" result
    0
