open System

type SquareRootResult =
    | NoRoots
    | OneRoot of double
    | TwoRoots of double * double 
    | ThreeRoots of double * double * double
    | FourRoots of double * double * double * double

let CalculateRoots (a: double, b: double, c: double): SquareRootResult =
    let D = b * b - 4.0 * a * c
    if D < 0.0 then
        NoRoots
    else

    if D = 0.0 then
        let rt = -b / (2.0 * a)
        if rt < 0.0 then
            NoRoots
        else
            let srt = Math.Sqrt(rt)
            if srt = 0.0 then OneRoot(srt) else TwoRoots(srt, -srt)
    else
        let sqrtD = Math.Sqrt(D)
        let rt1 = (-b + sqrtD) / (2.0 * a)
        let rt2 = (-b - sqrtD) / (2.0 * a)
        if rt1 < 0.0 && rt2 >= 0.0 then
            let srt2 = Math.Sqrt(rt2)
            if srt2 = 0.0 then OneRoot(srt2) else TwoRoots(srt2, -srt2)
        else if rt1 >= 0.0 && rt2 < 0.0 then
            let srt1 = Math.Sqrt(rt1)
            if srt1 = 0.0 then OneRoot(srt1) else TwoRoots(srt1, -srt1)
        else if rt1 < 0.0 && rt2 < 0.0 then
            NoRoots
        else
            let srt1 = Math.Sqrt(rt1)
            let srt2 = Math.Sqrt(rt2)
            FourRoots(srt1, srt2, -srt1, -srt2)



let PrintRoots (a: double, b: double, c: double): unit =
    printf "Коэффициенты: a=%A, b=%A, c=%A. " a b c
    let root = CalculateRoots(a, b, c)
   
    let textResult =
        match root with
        | NoRoots -> "Корни комплексные"
        | OneRoot (rt) -> "Один корень " + rt.ToString()
        | TwoRoots (rt1, rt2) ->
            "Два корня "
            + rt1.ToString()
            + " и "
            + rt2.ToString()
        | ThreeRoots (rt1, rt2, rt3) ->
            "3 корня "
            + rt1.ToString()
            + ", "
            + rt2.ToString()
            + " и "
            + rt3.ToString()
        | FourRoots (rt1, rt2, rt3, rt4) ->
            "4 корня "
            + rt1.ToString()
            + ", "
            + rt2.ToString()
            + ", "
            + rt3.ToString()
            + " и "
            + rt4.ToString()

    printfn "%s" textResult

[<EntryPoint>]
let main argv =

    let a1 = 1.0
    let b1 = 0.0
    let c1 = -4.0

    let a2 = 1.0
    let b2 = 0.0
    let c2 = 0.0

    let a3 = 1.0
    let b3 = 0.0
    let c3 = 4.0
    PrintRoots(a1, b1, c1)
    PrintRoots(a2, b2, c2)
    PrintRoots(a3, b3, c3)
    0
