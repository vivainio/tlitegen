module RegexUt

open System.Text.RegularExpressions

let reMatch s pat = Regex.Match(s, pat, RegexOptions.IgnoreCase).Success

let reMatchTries (tries: (string*'t) seq) input =
    tries |>
    Seq.tryFind (fun trie -> reMatch input (fst trie))

let anyReMatch (rxs: string seq) (s: string) = rxs |> Seq.exists (reMatch s)
