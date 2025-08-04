open System
open System.Security.Cryptography
open System.Text

let usage() =
    Console.Error.WriteLine("usage: dotnet run <input file>")
    Environment.Exit(1)

let processKey(key: string) =
    use md5 = MD5.Create()
    let rec f n =
        let tryKey = key + n.ToString()
        let inputBytes = Encoding.UTF8.GetBytes(tryKey)
        let hashBytes = md5.ComputeHash(inputBytes)
        let hexDigest = hashBytes |> Array.map (fun b -> b.ToString("x2")) |> String.Concat
        if hexDigest.StartsWith("000000") then
            n
        else
            f (n + 1)
    f 1

[<EntryPoint>]
let main args =
    if args.Length < 1 then
        usage()

    let key = args.[0]
    let result = processKey key
    Console.WriteLine("result = " + result.ToString());
    0 
