module f_sharp_training
open NUnit.Framework
open Swensen.Unquote
open SafetyFirst
open FSharpx

let euler1 upperBound =
    //example walkthrough
    let input = [1 .. (upperBound-1)]
    let isMultipleOf a b = (a % b = 0)
    let isMultipleOf3or5 i = (isMultipleOf i 3) || (isMultipleOf i 5)
    let three569 : int list =  List.filter isMultipleOf3or5 input
    in List.sum three569

let euler3 num = 
    //find largest prime factor of a given number
    let rec factorFind num startFactor factorList =
        if startFactor=num then
            startFactor::factorList
        elif num%startFactor=0 then
            factorFind (num/startFactor) startFactor (startFactor::factorList)
        else
            factorFind  num (startFactor+1) factorList //
    let primeList: int list = factorFind num 2 []
    List.max' (primeList) 

let euler8 (str: string) (window: int)  =
    // find the largest product of n sized window from given int
    result{
        let! windowedString = 
            str 
            |> Seq.toList
            |> List.map (fun x -> System.Char.GetNumericValue x) 
            |> List.windowed' window
            |> Result.mapError string 
        
        let nonEmptyWindowToList = [for i in windowedString do yield List.NonEmpty.toList i]
        let windowListToInt = [for i in nonEmptyWindowToList do yield [for j in i do yield System.Convert.ToInt32 j]]
        let windowProducts = [for i in windowListToInt do yield List.fold(*) 1 i]

        return! List.max'(windowProducts) |> Result.mapError string 
    }

let leetcode3 (string: string)= 
    let isEntirelyDistinct list = (List.length list) = (List.distinct list|> List.length)
    
    let listOfChars = string |> Seq.toList
    let lengthOfList = List.length listOfChars
    let windowSizes = [1..lengthOfList]
    
    let possibleWindows = [for i in windowSizes do yield List.windowed i listOfChars] |> List.concat
    let validWindows =  List.filter isEntirelyDistinct possibleWindows
    
    match List.last' validWindows with
        | Ok x -> x.Length
        | Error x -> 0

let advent2 (input: string) = 
    // calculate score for a given strategy
    let rounds = input.Split('\n') |> Array.map (fun elem -> (elem[0], elem[1]))
    let score = List.sum [for i in rounds -> match i with
                                                    | ('A','Y') -> 8 
                                                    | ('A','X') -> 4
                                                    | ('A','Z') -> 3
                                                    | ('B','Y') -> 5
                                                    | ('B','X') -> 1
                                                    | ('B','Z') -> 9
                                                    | ('C','Y') -> 2
                                                    | ('C','X') -> 7
                                                    | ('C','Z') -> 6
                                                    | (_,_) -> 3]
                            
    score


[<Test>]
let Test1 () =
    test <@euler3 20 = Ok 5 @> 

[<Test>]
let Test2 () =
    let testString = "73167176531330624919225119674426574742355349194934\
        96983520312774506326239578318016984801869478851843\
        85861560789112949495459501737958331952853208805511\
        12540698747158523863050715693290963295227443043557\
        66896648950445244523161731856403098711121722383113\
        62229893423380308135336276614282806444486645238749\
        30358907296290491560440772390713810515859307960866\
        70172427121883998797908792274921901699720888093776\
        65727333001053367881220235421809751254540594752243\
        52584907711670556013604839586446706324415722155397\
        53697817977846174064955149290862569321978468622482\
        83972241375657056057490261407972968652414535100474\
        82166370484403199890008895243450658541227588666881\
        16427171479924442928230863465674813919123162824586\
        17866458359124566529476545682848912883142607690042\
        24219022671055626321111109370544217506941658960408\
        07198403850962455444362981230987879927244284909188\
        84580156166097919133875499200524063689912560717606\
        05886116467109405077541002256983155200055935729725\
        71636269561882670428252483600823257530420752963450" 
    test <@euler8 testString 4 = Ok 5832 @>  

[<Test>]
let Test3 () =
    test <@ leetcode3 "abcabcbb" = 3 @>

[<Test>]
let Test4 () =
    test <@advent2 "AY\nBX\nCZ" = 15 @>