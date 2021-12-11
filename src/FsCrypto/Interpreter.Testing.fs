module FsCrypto.Interpreter.Testing

open System
open FsCrypto.Instructions
open FSharpPlus
open FSharpPlus.Data

let [<Literal>] private ExchangeName = "Testing"

let interpret instruction =
    match instruction with
    | GetBalances (_, next) -> async {
        do! Async.Sleep 2000 // Pretend it takes time

        let balances =
            [
                InstructionBalance.Create ExchangeName "BTC" 0.0175m
                InstructionBalance.Create ExchangeName "ETH" 0.03m
                InstructionBalance.Create ExchangeName "ADA" 200.123m
                InstructionBalance.Create ExchangeName "ALU" 123.123m
                InstructionBalance.Create ExchangeName "SHIB" 123456789.0m
            ]
        return balances |> next
        }

    | GetTransactions (_, next) -> async {
        do! Async.Sleep 4000 // Pretend it takes time

        let transactions =
            [
                let startDate = DateTimeOffset(DateTime(2021, 1, 1, 12, 00, 00))
                InstructionTransaction.Create ExchangeName (startDate.AddDays 1) (TransactionType.Deposit (InstructionAmount.Create 1000m "USD"))
                InstructionTransaction.Create ExchangeName (startDate.AddDays 2) (TransactionType.Transaction ((InstructionAmount.Create 0.0348m "BTC"), (InstructionAmount.Create 1000m "USD"), (InstructionAmount.Create 0.5m "USD")))
                InstructionTransaction.Create ExchangeName (startDate.AddDays 4) (TransactionType.Transaction ((InstructionAmount.Create 0.03m "ETH"), (InstructionAmount.Create 1000m "USD"), (InstructionAmount.Create 1m "USD")))
                InstructionTransaction.Create ExchangeName (startDate.AddMonths 11) (TransactionType.Transaction ((InstructionAmount.Create 932.4663m "USD"), (InstructionAmount.Create 0.0174m "BTC"), (InstructionAmount.Create 0.5m "USD")))
                InstructionTransaction.Create ExchangeName (startDate.AddMonths 12) (TransactionType.Withdrawal ((InstructionAmount.Create 932.4663m "USD"), (InstructionAmount.Create 1m "USD")))
            ]
        return transactions |> next
        }

    | GetMarkets (_, next) -> async {
        do! Async.Sleep 1000 // Pretend it takes time

        let markets =
            [
                InstructionMarket.Create "BTC" 500000m 500000m 500000m
                InstructionMarket.Create "ETH" 35000m 35000m 35000m
                InstructionMarket.Create "ADA" 8.25m 8.25m 8.25m
                InstructionMarket.Create "ALU" 1.34m 1.34m 1.34m
                InstructionMarket.Create "SHIB" 0.0003m 0.0003m 0.0003m
            ]
        return markets |> next
    }

let run (instructions:Free<CryptoInstruction<'a>, 'a>) =
    instructions
    |> Free.fold interpret