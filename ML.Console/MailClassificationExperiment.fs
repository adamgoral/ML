namespace ML.Console

module MailClassificationExperiment =
    open System
    open ML.Data.Outlook
    open ML.Classification

    let saveToFile fileName items =
        use writer = new System.IO.StreamWriter(System.IO.File.OpenWrite(fileName))
        Seq.iter (fun (words: string list, cat) -> writer.WriteLine("\"{0}\",\"{1}\"", cat, String.Join("\",\"", words))) items

    let dumpCategorisedEmailToFile() =
        let input = ML.Data.Outlook.Inbox.GetProcessedMail() |> Seq.map (fun mail -> mailToInput mail) |> List.ofSeq
        let input = seq {for (words, cats) in input do
                            for cat in cats do
                                yield (words, cat)}
        saveToFile "classification.csv" input |> ignore