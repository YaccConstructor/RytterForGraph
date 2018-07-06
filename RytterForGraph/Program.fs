// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let mult p1 p2 =
    match p1,p2 with
    | (x,y),(a,b) when y = a -> Some (x,b)
    | _ -> None

let multSet s1 s2 =
    s1 |> Seq.map (fun e1 -> s2 |> Set.map (fun e2 -> mult e1 e2) |> Set.filter (fun x -> x.IsSome) |> Set.map (fun x -> x.Value))
    |> Set.unionMany
        
let matSum (m1:Set<_> [,]) (m2:Set<_> [,]) =
    let res = Array2D.init (m1.GetLength 0) (m1.GetLength 0) (fun _ _ -> Set.empty)
    for i in 0..(m1.GetLength 0) - 1 do
        for j in 0..(m1.GetLength 0) - 1 do        
            res.[i,j] <- Set.union m1.[i,j] m2.[i,j]
    res

let matMultiply (m1:Set<_> [,]) (m2:Set<_> [,]) =
    let res = Array2D.init (m1.GetLength 0) (m1.GetLength 0) (fun _ _ -> Set.empty)
    for i in 0..(m1.GetLength 0) - 1 do        
        for j in 0..(m1.GetLength 0) - 1 do
            let mutable sum = Set.empty
            for k in 0..(m1.GetLength 0) - 1 do
                sum <- Set.union sum (multSet m1.[i,k] m2.[k,j])
            res.[i,j] <- sum
    res
    
let powM (m:Set<_> [,]) n =
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

let transpose (m:Set<_> [,]) = 
    let res = Array2D.init (m.GetLength 0) (m.GetLength 0) (fun _ _ -> Set.empty)
    m |> Array2D.iteri (fun i j n -> res.[j,i] <- n)
    res

let toDot graph file =
    let edgs = 
        graph
        |> Set.filter (fun (_,t,_) -> t <> "S1") 
        |> Set.map (fun (i,t,j) -> sprintf "%i -> %i [label=%s]" i j t)
        |> String.concat "\n"

    System.IO.File.WriteAllText(file, "digraph g{" + edgs + "}")

let eval graph grammar = 

    let mutable graph = graph

    let vrts = new System.Collections.Generic.HashSet<_>()

    let n = 
        graph |> Set.iter (fun (i,_,j) -> vrts.Add i |> ignore; vrts.Add j |> ignore)
        vrts.Count

    let Id = [|("A","A"); ("B","B"); ("S1","S1"); ("S","S")|] |> Set.ofArray    

    let mutable c = true

    let mutable steps = 0

    while c do
        //toDot graph (sprintf "dot/result_%i.dot" steps)
        let X,V,H =        
            let V = Array2D.init n n (fun _ _ -> Set.empty)
            let H = Array2D.init n n (fun _ _ -> Set.empty)
            let X = Array2D.init n n (fun _ _ -> Set.empty)
            for (i,t,j) in graph do
                X.[i,j] <- Set.add (t,t) X.[i,j]
                for (n1,n2,n3) in grammar do
                    if t = n2
                    then V.[i,j] <- Set.add (n3, n1) V.[i,j] 
                    if t = n3
                    then H.[i,j] <- Set.add (n2, n1) H.[i,j]
            for i in 0..n-1 do 
                H.[i,i] <- Set.union Id H.[i,i]
                V.[i,i] <- Set.union Id V.[i,i]

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
                    [
                        p1
                        p2
                    ]                
                    |> List.reduce matSum
                r X (k-1)
                
        let r = r X (int (System.Math.Log(float (n*n), 2.0)) + 1)
        let oldSize = graph.Count
        r |> Array2D.iteri(fun i j n ->  graph <- Set.union (n |> Set.map (fun (_,n1) -> (i,n1,j))) graph)
                   
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
    let n = 16
    let mutable graph = 
        [|
            (0,"A",1)
            (1,"A",8)
            (8,"A",9)
            (9,"A",10)
            (10,"A",11)
            (11,"A",12)
            (12,"A",2)
            (2,"A",3)
            (3,"A",0)
            (3,"B",4)
            (4,"B",5)
            (5,"B",6)
            (6,"B",7)
            (7,"B",13)
            (13,"B",14)
            (14,"B",15)
            (15,"B",3)
        |]
//        [|
//            (0,"A",1)
//            (1,"A",0)
//            (1,"B",2)
//            (2,"B",3)
//            (3,"B",1)
//        |]
        |> Set.ofArray

    let grammar =
        [|
            ("S","A","S1")
            ("S1","S","B")
            ("S","A","B")
        |]
//        ,[|
//            ("A","a")
//            ("B","b")
//        |]

    //let g = genGraph 2
    //toDot g "r.dot" 
    for i in 2 .. 5 .. 100 do
        let graph = genGraph i
        let graph,steps = eval graph grammar
        printfn "Total steps for n=%A = %A" (2*(i+1)+1) steps
    //toDot graph "result.dot" 

    0 // return an integer exit code
