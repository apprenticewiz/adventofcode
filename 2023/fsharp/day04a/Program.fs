open System
open System.IO

let usage () =
    let progname = Path.GetFileName(Environment.GetCommandLineArgs().[0])
    printfn "usage: %s <file>" progname
    Environment.Exit(1)

let processInput (contents: string) =
    Seq.fold
        (fun (result: uint) (line: string) ->
            let rest = line.Split(": ")[1]
            let winningStr = rest.Split(" | ")[0]
            let winningSet =
                Seq.fold
                    (fun set numStr ->
                        let num = Int32.Parse(numStr)
                        Set.add num set)
                    Set.empty
                    (winningStr.Split (" ", StringSplitOptions.RemoveEmptyEntries))
            let handStr = rest.Split(" | ")[1]
            let handSet =
                Seq.fold
                    (fun set numStr ->
                        let num = Int32.Parse(numStr)
                        Set.add num set)
                    Set.empty
                    (handStr.Split (" ", StringSplitOptions.RemoveEmptyEntries))
            let intersection = Set.intersect winningSet handSet
            let count = Set.count intersection
            result + if (count > 0) then (pown 2u (count - 1)) else 0u)
        0u
        (contents.Split ('\n', StringSplitOptions.RemoveEmptyEntries))

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        usage()
    let filename = argv.[0]
    let contents = File.ReadAllText(filename)
    let result = processInput contents
    printfn "result = %d" result
    0
