// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Collections.Generic

let inline mult p1 p2 =
    match p1,p2 with
    | (x,y),(a,b) when y = a -> Some (x,b)
    | _ -> None

let inline multSet s1 s2 =
    let res = new HashSet<_>()
    s1 |> Seq.iter (fun e1 -> s2 |> Seq.iter (fun e2 -> 
            let r = mult e1 e2
            if r.IsSome
            then res.Add r.Value |> ignore))
    res
        
let matSum (m1:HashSet<_> [,]) (m2:HashSet<_> [,]) =
    let res = Array2D.init (m1.GetLength 0) (m1.GetLength 0) (fun _ _ -> new HashSet<_>())
    for i in 0..(m1.GetLength 0) - 1 do
        for j in 0..(m1.GetLength 0) - 1 do        
            res.[i,j].UnionWith m1.[i,j]
            res.[i,j].UnionWith m2.[i,j]
    res

let matMultiply (m1:HashSet<_> [,]) (m2:HashSet<_> [,]) =
    let res = Array2D.init (m1.GetLength 0) (m1.GetLength 0) (fun _ _ -> new HashSet<_>())
    [|0..(m1.GetLength 0) - 1|]
    |> Array.Parallel.iter(fun i ->        
        for j in 0..(m1.GetLength 0) - 1 do
            let mutable sum = new HashSet<_>()
            for k in 0..(m1.GetLength 0) - 1 do
                sum.UnionWith (multSet m1.[i,k] m2.[k,j])
            res.[i,j] <- sum)
    res
    
let powM (m:HashSet<_> [,]) n =
    let mutable steps = 0
    let rec _go _m _n =
        if _n >= n
        then _m
        else
            steps <- steps + 1
            _go (matMultiply m m) (_n*2)

    let res =_go m 1
    //printfn "Steps in powM = %A" steps
    res

let transpose (m:HashSet<_> [,]) = 
    let res = Array2D.init (m.GetLength 0) (m.GetLength 0) (fun _ _ -> new HashSet<_>())
    m |> Array2D.iteri (fun i j n -> res.[j,i] <- n)
    res

let toDot graph file =
    let edgs = 
        graph
        |> Set.filter (fun (_,t,_) -> t <> "S1") 
        |> Set.map (fun (i,t,j) -> sprintf "%i -> %i [label=%s]" i j t)
        |> String.concat "\n"

    System.IO.File.WriteAllText(file, "digraph g{" + edgs + "}")

let eval (graph:seq<_>) grammar = 

    let graph = new HashSet<_>(graph)

    let vrts = new System.Collections.Generic.HashSet<_>()

    let n = 
        graph |> Seq.iter (fun (i,_,j) -> vrts.Add i |> ignore; vrts.Add j |> ignore)
        vrts.Count

    let Id = new HashSet<_> [|("A","A"); ("B","B"); ("S1","S1"); ("S","S")|] 

    let mutable c = true

    let mutable steps = 0

    while c do
        //toDot graph (sprintf "dot/result_%i.dot" steps)
        let X,V,H =        
            let V = Array2D.init n n (fun _ _ -> new HashSet<_>())
            let H = Array2D.init n n (fun _ _ -> new HashSet<_>())
            let X = Array2D.init n n (fun _ _ -> new HashSet<_>())
            for (i,t,j) in graph do
                X.[i,j].Add(t,t)  |> ignore
                for (n1,n2,n3) in grammar do
                    if t = n2
                    then V.[i,j].Add(n3, n1) |> ignore
                    if t = n3
                    then H.[i,j].Add (n2, n1) |> ignore
            for i in 0..n-1 do 
                H.[i,i].UnionWith Id
                V.[i,i].UnionWith Id

            X,V,H

        let Vn = powM V (n*n)
        let Hn = powM H (n*n)

        let t = transpose
        let m = matMultiply

        let rec r X k = 
            if k = 0 
            then X
            else 
                let X = 
                    let p1 =  (m (t (m (t X) (t Vn))) Hn)
                    let p2 = t (m (t (m X Hn)) (t Vn))
                    matSum p1 p2
                r X (k-1)
                
        let r = r X (int (System.Math.Log(float (n*n), 2.0)) * int (System.Math.Log(float (n*n), 2.0)))
        let oldSize = graph.Count
        r |> Array2D.iteri(fun i j n ->  graph.UnionWith (n |> Seq.map (fun (_,n1) -> (i,n1,j))))
                   
        //printfn "%A" graph
        
        steps <- steps + 1

        c <- oldSize <> graph.Count
    graph,steps

let genGraph n =
    (2 * n + 1, "B", n) :: (n,"A",0)::[for i in 0 .. n - 1 -> (i, "A", i+1)]
    @ [for i in 0 .. n -> (n + i, "B", n + i + 1)]
    |> Set.ofList

[<EntryPoint>]
let main argv =     
    let grammar =
        [|
            ("S","A","S1")
            ("S1","S","B")
            ("S","A","B")
        |]

    for i in 2 .. 1 .. 100 do
        let graph = genGraph i
        let graph,steps = eval graph grammar
        printfn "Total steps for n=%A = %A" (2*(i+1)+1) steps
    //toDot graph "result.dot" 

    0 // return an integer exit code
