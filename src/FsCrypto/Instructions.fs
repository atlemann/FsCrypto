module FsCrypto.Instructions

open System
open FSharpPlus.Data

type InstructionBalance =
    { Exchange: string
      Crypto: string
      Amount: decimal }
    static member Create exchange crypto amount =
        { Exchange = exchange
          Crypto = crypto
          Amount = amount }

type InstructionAmount =
    { Amount: decimal
      Currency: string }
    static member Create amount currency =
        { Amount = amount
          Currency = currency }

[<RequireQualifiedAccess>]
type TransactionType =
    | Deposit of Amount:InstructionAmount
    | Transaction of Bought:InstructionAmount * Sold:InstructionAmount * Fee:InstructionAmount
    | Withdrawal of Amount:InstructionAmount * Fee:InstructionAmount

type InstructionTransaction =
    { Exchange: string
      TimeStamp: DateTimeOffset
      Type: TransactionType }
    static member Create exchange timeStamp transactionType =
        { Exchange = exchange
          TimeStamp = timeStamp
          Type = transactionType }

type InstructionMarket =
    { MarketPair: string
      Last: decimal
      High: decimal
      Low: decimal }
    static member Create marketPair last high low =
        { MarketPair = marketPair
          Last = last
          High = high
          Low = low }

type CryptoInstruction<'a> =
    | GetBalances of unit * (InstructionBalance list -> 'a)
    | GetTransactions of unit * (InstructionTransaction list -> 'a)
    | GetMarkets of unit * (InstructionMarket list -> 'a)
    static member Map (op, f) =
        match op with
        | GetBalances (x, next) -> GetBalances (x, next >> f)
        | GetTransactions (x, next) -> GetTransactions (x, next >> f)
        | GetMarkets (x, next) -> GetMarkets (x, next >> f)

let getBalances () = GetBalances ((), id) |> Free.liftF
let getTransactions () = GetTransactions ((), id) |> Free.liftF
let getMarkets () = GetMarkets ((), id) |> Free.liftF
