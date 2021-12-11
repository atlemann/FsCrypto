open Argu
open Coinbase
open FsCrypto.Api.Firi
open FsCrypto.Instructions
open FsCrypto.Interpreter
open FSharp.Control
open FSharpPlus
open FSharpPlus.Data
open Spectre.Console
open System

let logo = """
 ________           ______                                  __
|        \         /      \                                |  \
| $$$$$$$$_______ |  $$$$$$\  ______   __    __   ______  _| $$_     ______
| $$__   /       \| $$   \$$ /      \ |  \  |  \ /      \|   $$ \   /      \
| $$  \ |  $$$$$$$| $$      |  $$$$$$\| $$  | $$|  $$$$$$\\$$$$$$  |  $$$$$$\
| $$$$$  \$$    \ | $$   __ | $$   \$$| $$  | $$| $$  | $$ | $$ __ | $$  | $$
| $$     _\$$$$$$\| $$__/  \| $$      | $$__/ $$| $$__/ $$ | $$|  \| $$__/ $$
| $$    |       $$ \$$    $$| $$       \$$    $$| $$    $$  \$$  $$ \$$    $$
 \$$     \$$$$$$$   \$$$$$$  \$$       _\$$$$$$$| $$$$$$$    \$$$$   \$$$$$$
                                      |  \__| $$| $$
                                       \$$    $$| $$
                                        \$$$$$$  \$$
"""

let spinner =
    let spinner = AnsiConsole.Status()
    spinner.Spinner <- Spinner.Known.Dots
    spinner.SpinnerStyle <- Style.Parse("green bold")
    spinner

module Program =

    type Run<'a> = Free<CryptoInstruction<'a list>, 'a list> -> Async<Result<'a list, string list>>

    open FsToolkit.ErrorHandling

    let run (firiApi:FiriApi option) (coinbaseClient:CoinbaseClient option) program =
        asyncSeq {
            //yield (Testing.run >> Async.map Ok)

            if firiApi.IsSome then
                yield Firi.run firiApi.Value

            if coinbaseClient.IsSome then
                yield (Coinbase.run coinbaseClient.Value >> Async.map Ok)
        }
        |> AsyncSeq.mapAsyncParallel (fun run -> run program)
        |> AsyncSeq.toListAsync
        |> Async.map (List.sequenceResultA >> Result.map (List.collect id))

let showTransactions (run:Program.Run<_>) = async {

    let getTransactionsProgram = monad {
            let! transactions = getTransactions ()
            return transactions
        }

    let! transactions =
        spinner.StartAsync(
            "[green]Getting transactions...[/]",
            fun _ -> run getTransactionsProgram |> Async.StartAsTask)
        |> Async.AwaitTask

    match transactions with
    | Ok ts ->
        let table =
            Table()
                .AddColumn("[blue]Type[/]")
                .AddColumn("[blue]Date[/]")
                .AddColumn("[blue]Buy[/]")
                .AddColumn("[blue]Currency[/]")
                .AddColumn("[blue]Sell[/]")
                .AddColumn("[blue]Currency[/]")
                .AddColumn("[blue]Fee[/]")
                .AddColumn("[blue]Currency[/]")
                .AddColumn("[blue]Exchange[/]")
        table.Border <- TableBorder.Rounded
        table.Title <- TableTitle("[purple_1]Transaction history[/]")
        table.Columns.[2].Alignment <- Justify.Right
        table.Columns.[4].Alignment <- Justify.Right
        table.Columns.[6].Alignment <- Justify.Right

        let addRow typeString (timeStamp:DateTimeOffset) bought sold fee exchange =
            table.AddRow (
                typeString,
                $"""[orange1]{timeStamp.ToString("u")}[/]""",
                bought |> Option.map (fun b -> $"[green]{b.Amount}[/]") |> Option.defaultValue "",
                bought |> Option.map (fun b -> $"[blue]{b.Currency}[/]") |> Option.defaultValue "",
                sold |> Option.map (fun b -> $"[red]{b.Amount}[/]") |> Option.defaultValue "",
                sold |> Option.map (fun b -> $"[blue]{b.Currency}[/]") |> Option.defaultValue "",
                fee |> Option.map (fun b -> $"[red]{b.Amount}[/]") |> Option.defaultValue "",
                fee |> Option.map (fun b -> $"[blue]{b.Currency}[/]") |> Option.defaultValue "",
                $"[deepskyblue1]{exchange}[/]")
            |> ignore

        ts
        |> List.sortBy (fun x -> x.TimeStamp)
        |> List.iter (fun t ->
            match t.Type with
            | TransactionType.Deposit amount ->
                addRow "[green]Deposit[/]" t.TimeStamp (Some amount) None None t.Exchange
            | TransactionType.Transaction (bought, sold, fee) ->
                addRow "[yellow]Transaction[/]" t.TimeStamp (Some bought) (Some sold) (Some fee) t.Exchange
            | TransactionType.Withdrawal (amount, fee) ->
                addRow "[red]Withdrawal[/]" t.TimeStamp None (Some amount) (Some fee) t.Exchange
            |> ignore)

        AnsiConsole.Write (table)

    | Error err ->
        AnsiConsole.MarkupLine($"[Red]Failed to get transactions. Details: {err}[/]")
}

let showBalances (run:Program.Run<_>) = async {

    let getBalancesProgram = monad {
        let! balances = getBalances ()
        let! markets = getMarkets ()

        let marketLookup =
            ("NOK", 1.0m) ::
            (markets |> List.map (fun m -> (m.MarketPair, m.Last)))
            |> Map.ofList

        let nokPerCoin =
            balances
            |> List.map (fun b ->
                let value =
                    marketLookup
                    |> Map.find b.Crypto

                (b.Crypto, b.Amount * value))
            |> Map.ofList

        return
            balances
            |> List.map (fun b ->
                {|
                    b with
                        Value = nokPerCoin.[b.Crypto]
                        Currency = "NOK"
                |})
        }

    let! balances =
        spinner.StartAsync(
            "[green]Getting balances...[/]",
            fun _ -> run getBalancesProgram |> Async.StartAsTask)
        |> Async.AwaitTask

    match balances with
    | Ok xs ->
        let table =
            Table()
                .AddColumn("[blue]Crypto[/]")
                .AddColumn("[blue]Amount[/]")
                .AddColumn("[blue]Value[/]")
                .AddColumn("[blue]Currency[/]")
                .AddColumn("[blue]Exchange[/]")
        table.Border <- TableBorder.Rounded
        table.Title <- TableTitle("[purple_1]Balances[/]")
        table.Columns.[1].Alignment <- Justify.Right
        table.Columns.[2].Alignment <- Justify.Right

        xs
        |> List.filter (fun t -> t.Amount > 0.0m)
        |> List.iter (fun t ->
            table.AddRow(
                $"[orange1]{t.Crypto}[/]",
                $"[green]%.4f{t.Amount}[/]",
                $"[green]%.2f{t.Value}[/]",
                $"[blue]{t.Currency}[/]",
                $"[deepskyblue1]{t.Exchange}[/]")
            |> ignore)

        let sum = xs |> List.sumBy (fun t -> t.Value)
        table.AddRow("[lightgreen]Sum[/]", "", $"[lightgreen]%.2f{sum}[/]", "[blue]NOK[/]") |> ignore

        AnsiConsole.Write (table)

    | Error err ->
        AnsiConsole.MarkupLine($"[Red]Failed to get balances. Details: {err}[/]")
    }

[<RequireQualifiedAccess>]
type Selection =
    | Transactions
    | Balances
    | Quit

type CLIArguments =
    | Firi of ClientId:string * Secret:string
    | Coinbase of ApiKey:string * ApiSecret:string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Firi _ -> "Use Firi"
            | Coinbase _ -> "Use Coinbase"

[<EntryPoint>]
let main argv =

    AnsiConsole.Markup $"[mediumpurple3_1]{logo}[/]"

    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<CLIArguments>(programName = "FsCrypto", errorHandler = errorHandler)
    let parsedArgs = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

    let firiApiOpt =
        parsedArgs.TryGetResult CLIArguments.Firi
        |> Option.map (fun (firiClientId, firiSecret) ->
            new FiriApi(FiriSecrets.Create firiClientId firiSecret))

    let coinbaseClientOpt =
        parsedArgs.TryGetResult CLIArguments.Coinbase
        |> Option.map (fun (coinbaseApiKey, coinbaseApiSecret) ->
            new CoinbaseClient(new ApiKeyConfig(ApiKey=coinbaseApiKey, ApiSecret=coinbaseApiSecret)))

    if firiApiOpt.IsNone && firiApiOpt.IsNone then
        let usage = parser.PrintUsage()
        printfn "%s" usage
    else

        let selectionPrompt =
            let prompt = new SelectionPrompt<Selection>()
            prompt.Title <- "[blue]What do you want to see?[/]"
            prompt.AddChoices(
                [|
                    Selection.Transactions
                    Selection.Balances
                    Selection.Quit
                |]) |> ignore
            prompt

        let rec loop () = async {

            let choice = AnsiConsole.Prompt(selectionPrompt)
            match choice with
            | Selection.Transactions ->
                let runner = Program.run firiApiOpt coinbaseClientOpt
                do! showTransactions runner
                return! loop ()

            | Selection.Balances ->
                let runner = Program.run firiApiOpt coinbaseClientOpt
                do! showBalances runner
                return! loop ()

            | Selection.Quit ->
                ()
            }

        loop ()
        |> Async.RunSynchronously

    0 // return an integer exit code