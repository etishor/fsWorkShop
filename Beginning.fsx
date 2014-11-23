  
open System
open System.IO

type BitmapData = { Number:int; Pixels: int[] }

let readFile path = File.ReadAllLines path
let skipHeader (a:string[]) = a.[1..]
let splitCSVString (s:string) = s.Split ','
let mapToInteger line = Array.map (fun s -> int s) line
let convertToBitmap (data : int[])  = { Number = data.[0]; Pixels = data.[1..] }  

let readCSVFile path =
    __SOURCE_DIRECTORY__ + Path.DirectorySeparatorChar.ToString() + path 
    |> readFile
    |> skipHeader 
    |> Array.Parallel.map splitCSVString
    |> Array.Parallel.map mapToInteger
    |> Array.Parallel.map convertToBitmap

let pow2difference p1 p2 = pown (p1 - p2) 2

let euclideanDistance (first : int[]) (second : int[]) =
    Array.map2 pow2difference first second 
    |> Array.sum
    |> float
    |> Math.Sqrt

let distanceFrom (data : BitmapData) (target : BitmapData)  = 
    euclideanDistance target.Pixels data.Pixels


let trainingData = readCSVFile "trainingsample.csv"
let validationData = readCSVFile "validationsample.csv"
 
let clasify (bitmap: BitmapData) =
    trainingData  
    |> Array.minBy (distanceFrom bitmap)

let compare (data : BitmapData) =
    let result = clasify data
    if data.Number = result.Number then 1. else 0.
 
let result = 
    validationData 
    |> Array.Parallel.map compare
    |> Array.average