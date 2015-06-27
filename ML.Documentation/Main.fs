module Main
open FsLab
open FSharp.Literate
open System.IO
open System

let processExperiment name outputKind =
  let source = __SOURCE_DIRECTORY__
  let htmlTemplateFile = Path.Combine(source, @"Packages\FsLab.Runner\styles\Template.html")
  let outputPath = Path.Combine(source, @"output\" + name + ".html")
  let sourceFile = Path.Combine(source, name + ".fsx")
  printf "Creating %s..." name
  Literate.ProcessScriptFile(sourceFile, templateFile = htmlTemplateFile, format = outputKind, output = outputPath)
  printfn "done"


[<EntryPoint>]
let main args = 
  // Usage:
  //
  //  --latex              Generate output as LaTeX rather than the default HTML
  //  --non-interactive    Do not open the generated HTML document in web browser
  //
  let latex = args |> Seq.exists ((=) "--latex")
  let browse = args |> Seq.exists ((=) "--non-interactive") |> not
  let outputKind = if latex then OutputKind.Latex
                   else OutputKind.Html
  try
    //processExperiment "EmailClassifierBPM" OutputKind.Latex
    //printfn "Completed"
    Journal.Process(browse = browse, outputKind = outputKind)
    0
  with
    | ex -> Console.WriteLine("Error processing journal {0}\nPress ENTER to exit.", ex)
            if Environment.UserInteractive then Console.ReadLine() |> ignore
            1
  0
