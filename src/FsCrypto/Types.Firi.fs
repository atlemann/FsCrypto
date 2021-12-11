module FsCrypto.Types.Firi

open System
open Thoth.Json.Net

type OrderType =
    | Bid
    | Ask
    member this.AsString =
        match this with
        | Ask -> "Ask"
        | Bid -> "Bid"

type OrderRequest =
    { Market: string
      Type: OrderType
      Price: decimal
      Amount: decimal }
    static member Create market orderType price amount =
        { Market = market
          Type = orderType
          Price = price
          Amount = amount }

type Market =
    { Id: string
      Last: decimal
      High: decimal
      Change: decimal
      Low: decimal
      Volume: decimal }
    static member Decoder : Decoder<Market> =
        Decode.object
            (fun get ->
                { Id = get.Required.Field "id" Decode.string
                  Last = get.Required.Field "last" Decode.string |> decimal
                  High = get.Required.Field "high" Decode.string |> decimal
                  Change = get.Required.Field "change" Decode.string |> decimal
                  Low = get.Required.Field "low" Decode.string |> decimal
                  Volume = get.Required.Field "volume" Decode.string |> decimal })

type Balance =
    { Currency: string
      Balance: decimal
      Hold: decimal
      Available: decimal }
    static member Decoder : Decoder<Balance> =
        Decode.object
            (fun get ->
                { Currency = get.Required.Field "currency" Decode.string
                  Balance = get.Required.Field "balance" Decode.string |> decimal
                  Hold = get.Required.Field "hold" Decode.string |> decimal
                  Available = get.Required.Field "available" Decode.string |> decimal })

type Trade =
    { Id: Guid
      Market: string
      Price: decimal
      PriceCurrency: string
      Amount: decimal
      AmountCurrency: string
      Cost: decimal
      CostCurrency: string
      Side: string
      IsMaker: bool
      Date: DateTimeOffset }
    static member Decoder : Decoder<Trade> =
        Decode.object
            (fun get ->
                { Id = get.Required.Field "id" Decode.guid
                  Market = get.Required.Field "market" Decode.string
                  Price = get.Required.Field "price" Decode.string |> decimal
                  PriceCurrency = get.Required.Field "price_currency" Decode.string
                  Amount = get.Required.Field "amount" Decode.string |> decimal
                  AmountCurrency = get.Required.Field "amount_currency" Decode.string
                  Cost = get.Required.Field "cost" Decode.string |> decimal
                  CostCurrency = get.Required.Field "cost_currency" Decode.string
                  Side = get.Required.Field "side" Decode.string
                  IsMaker = get.Required.Field "isMaker" Decode.bool
                  Date = get.Required.Field "date" Decode.datetimeOffset })



type Transaction =
    { Id: Guid
      Amount: decimal
      Currency: string
      Type: string
      Date: DateTimeOffset
      Details: TransactionDetails }
    static member Decoder : Decoder<Transaction> =
        Decode.object
            (fun get ->
                { Id = get.Required.Field "id" Decode.guid
                  Amount = get.Required.Field "amount" Decode.string |> decimal
                  Currency = get.Required.Field "currency" Decode.string
                  Type = get.Required.Field "type" Decode.string
                  Date = get.Required.Field "date" Decode.datetimeOffset
                  Details = get.Required.Field "details" TransactionDetails.Decoder })
and TransactionDetails =
    { MatchId: string option
      DepositId: string option
      DepositAddress: string option
      DepositTxid: string option
      WithdrawId: string option
      WithdrawAddress: string option
      WithdrawTxid: string option }
    static member Decoder : Decoder<TransactionDetails> =
        Decode.object
            (fun get ->
                { MatchId = get.Optional.Field "match_id" Decode.string
                  DepositId = get.Optional.Field "deposit_id" Decode.string
                  DepositAddress = get.Optional.Field "deposit_address" Decode.string
                  DepositTxid = get.Optional.Field "deposit_txid" Decode.string
                  WithdrawId = get.Optional.Field "withdraw_id" Decode.string
                  WithdrawAddress = get.Optional.Field "withdraw_address" Decode.string
                  WithdrawTxid = get.Optional.Field "withdraw_txid" Decode.string })

type DepositHistory =
    { Count: int64
      Transactions: DepositTransactions list }
    static member Decoder : Decoder<DepositHistory> =
        Decode.object
            (fun get ->
                { Count = get.Required.Field "count" Decode.int64
                  Transactions = get.Required.Field "transactions" (Decode.list DepositTransactions.Decoder) })
and DepositTransactions =
    { Id: int64
      Amount: decimal
      Currency: string
      DepositedAt: DateTimeOffset
      TransactionHash: Guid option
      Status: string
      Confirmations: int
      Required: int }
    static member Decoder : Decoder<DepositTransactions> =
        Decode.object
            (fun get ->
                { Id = get.Required.Field "id" Decode.int64
                  Amount = get.Required.Field "amount" Decode.string |> decimal
                  Currency = get.Required.Field "currency" Decode.string
                  DepositedAt = get.Required.Field "deposited_at" Decode.datetimeOffset
                  TransactionHash = get.Optional.Field "transaction_hash" Decode.guid
                  Status = get.Required.Field "status" Decode.string
                  Confirmations = get.Required.Field "confirmations" Decode.int
                  Required = get.Required.Field "required" Decode.int }
            )

type Order =
    { Id: Guid
      Market: string
      Type: string
      Price: decimal
      PriceCurrency: string
      Amount: decimal
      AmountCurrency: string
      Matched: decimal
      Cancelled: decimal
      CreatedAt: DateTimeOffset }
    static member Decoder : Decoder<Order> =
        Decode.object
            (fun get ->
            { Id = get.Required.Field "id" Decode.guid
              Market = get.Required.Field "market" Decode.string
              Type = get.Required.Field "type" Decode.string
              Price = get.Required.Field "price" Decode.string |> decimal
              PriceCurrency = get.Required.Field "price_currency" Decode.string
              Amount = get.Required.Field "amount" Decode.string |> decimal
              AmountCurrency = get.Required.Field "amount_currency" Decode.string
              Matched = get.Required.Field "matched" Decode.string |> decimal
              Cancelled = get.Required.Field "cancelled" Decode.string |> decimal
              CreatedAt = get.Required.Field "created_at" Decode.datetimeOffset })

type CreateOrderResponse =
    { Id: string }
    static member Decoder : Decoder<CreateOrderResponse> =
        Decode.object
            (fun get ->
                { Id = get.Required.Field "id" Decode.string })