module FsCrypto.Hashing

open System
open System.Text
open System.Security.Cryptography

let computeSignature (secret:string) (data:string) =
    use hmacsha256 =
        secret
        |> Encoding.UTF8.GetBytes
        |> fun bs -> new HMACSHA256(bs)

    data
    |> Encoding.UTF8.GetBytes
    |> hmacsha256.ComputeHash
    |> Array.map (fun (x : byte) -> sprintf "%02x" x)
    |> String.concat String.Empty