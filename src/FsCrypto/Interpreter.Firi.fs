module FsCrypto.Interpreter.Firi

open FsCrypto.Api.Firi
open FsCrypto.Instructions
open FsCrypto.Types.Firi
open FSharpPlus
open FSharpPlus.Data
open FsToolkit.ErrorHandling

let [<Literal>] private ExchangeName = "Firi"

let private getBalancesApi (firiApi:FiriApi) =
    asyncResult {
        let! balances = firiApi.GetBalances()
        return
            balances
            |> List.map (fun b ->
                InstructionBalance.Create ExchangeName b.Currency b.Balance)
    }

module TransactionType =

    let private createTransaction = InstructionTransaction.Create ExchangeName

    let private createMatch (ts:Transaction list) =
        let bught =
            ts
            |> List.pick (fun t ->
                match t.Type, t.Amount with
                | "Match", x when x > 0.0m ->
                    Some (InstructionAmount.Create t.Amount t.Currency)
                | _ ->
                    None)

        let sold =
            ts
            |> List.pick (fun t ->
                match t.Type, t.Amount with
                | "Match", x when x < 0.0m ->
                    Some (InstructionAmount.Create t.Amount t.Currency)
                | _ ->
                    None)

        let fee =
            ts
            |> List.pick (fun t ->
                match t.Type with
                | "MatchFee" ->
                    Some (InstructionAmount.Create t.Amount t.Currency)
                | _ ->
                    None)

        TransactionType.Transaction (bught, sold, fee)
        |> createTransaction ts.Head.Date

    let private getMatches (transactions:Transaction list) =
        transactions
        |> List.filter (fun t -> t.Details.MatchId.IsSome)
        |> List.groupBy (fun t -> t.Details.MatchId)
        |> List.map (snd >> createMatch)

    let private getDeposits (transactions:Transaction list) =
        transactions
        |> List.choose (fun t ->
            if t.Type.Contains("Deposit") then
                InstructionAmount.Create t.Amount t.Currency
                |> TransactionType.Deposit
                |> createTransaction t.Date
                |> Some
            else
                None)

    let private createWithdrawal (transactions:Transaction list) =
        let amount =
            transactions
            |> List.pick (fun t ->
                if t.Type = "Withdraw"
                then Some (InstructionAmount.Create t.Amount t.Currency)
                else None)

        let fee =
            transactions
            |> List.pick (fun t ->
                if t.Type = "WithdrawFee"
                then Some (InstructionAmount.Create t.Amount t.Currency)
                else None)

        TransactionType.Withdrawal (amount, fee)
        |> createTransaction transactions.Head.Date

    let private getWithdrawals transactions =
        transactions
        |> List.filter (fun t -> t.Details.WithdrawId.IsSome)
        |> List.groupBy (fun t -> t.Details.WithdrawId)
        |> List.map (snd >> createWithdrawal)

    let groupTransactions transactions =
        [
            yield! getMatches transactions
            yield! getDeposits transactions
            yield! getWithdrawals transactions
        ]
        |> List.sortBy (fun t -> t.TimeStamp)

let private getTransactionsApi (firiApi:FiriApi) =
    asyncResult {
        let! transactions = firiApi.GetTransactions ()
        return TransactionType.groupTransactions transactions
    }

let private getMarketsApi (firiApi:FiriApi) =
    asyncResult {
        let! markets = firiApi.GetMarkets()
        return
            markets
            |> List.map (fun b -> InstructionMarket.Create (b.Id.Replace("NOK", "")) b.Last b.High b.Low)
    }

let interpret firiApi instruction =
    match instruction with
    | GetBalances (_, next) ->
        asyncResult {
            let! balances = getBalancesApi firiApi
            return balances |> next
        }
    | GetTransactions (_, next) ->
        asyncResult {
            let! buys = getTransactionsApi firiApi
            return buys |> next
        }
    | GetMarkets (_, next) ->
        asyncResult {
            let! markets = getMarketsApi firiApi
            return markets |> next
        }

let run firiApi (instructions:Free<CryptoInstruction<'a>, 'a>) =
    instructions
    |> Free.fold (interpret firiApi >> ResultT)
    |> ResultT.run
