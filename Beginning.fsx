  
open System
open System.IO

let skipHeader (a:string[]) = a.[ 1 .. a.Length-1 ]
let splitCSVString (s:string) = s.Split ','

let convertToInt (s:string) = int s
let convertToLineOfInts s = Array.map convertToInt s

type BitmapData = { Number:int; Pixels: int[] }

let convertToBitmap (data : int[])  = { Number = data.[0]; Pixels = data.[ 1 .. data.Length-1 ] }  

let readCSVFile path =
    File.ReadAllLines path
    |> skipHeader 
    |> Array.map splitCSVString
    |> Array.map convertToLineOfInts
    |> Array.map convertToBitmap

let trainingData = readCSVFile @"/Users/iulianmargarintescu/Projects/Digits/trainingsample.csv"

let matchPoints p1 p2 = (p1,p2)

let pow2difference p1 p2 = (p1 - p2) * (p1 - p2)

let euclideanDistance (first : int[]) (second : int[]) =
    Array.map2 pow2difference first second 
    |> Array.sum
    |> float
    |> Math.Sqrt

let distanceFrom (data : BitmapData) (target : BitmapData)  = 
    euclideanDistance target.Pixels data.Pixels
 
let clasify (bitmap: BitmapData) =
    let distance = distanceFrom bitmap

    let found = 
        trainingData
        |> Array.minBy distance

    found.Number


let validationData = readCSVFile @"/Users/iulianmargarintescu/Projects/Digits/validationsample.csv"

let compare (data : BitmapData) =
    let result = clasify data
    if data.Number = result then 1. else 0.
 
let matches = 
    validationData 
    |> Array.map compare
    |> Array.average

