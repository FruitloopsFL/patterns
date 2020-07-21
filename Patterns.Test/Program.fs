type Cell =
    |Positive
    |Negative
    |Unknown

type Pattern =
    |BlackP
    |WhiteP
    |UnknownP
    |ZeroOrMore of Pattern
    |OneOrMore of Pattern
    |Exactly of int*Pattern
    |FewerThan of int*Pattern
    |Sequence of List<Pattern>
    |Either of Pattern*Pattern
    |Anything
    |EndOfCells


let find pattern cellList =
    let rec recFind patt lst pos =
        match lst with
        |[] -> None
        |head::tail -> 
            match patternMatch patt lst with    
            |Some(x) -> Some(x, pos)
            |None -> recFind patt tail (pos + 1)
    recFind pattern cellList 0
            

let map someFunc pattern cellList =
    let rec mapthis recList =
        match recList with
        |head::tail -> 
            match find pattern recList with
            |Some(x, y) -> (List.truncate y recList) :: (someFunc x) :: (mapthis (List.skip ((List.length x) + y) recList))
            |None -> [recList]
        |[] -> []
    match find pattern cellList with
        |None -> cellList
        |Some(_, _) -> List.collect id (mapthis cellList)


let toCells k = 
    List.map (fun x -> match x with
        |'b' -> Positive
        |'B' -> Positive
        |'w' -> Negative
        |'W' -> Negative
        |_ -> Unknown) (Seq.toList k)


let fromCells k =
    let listChar = 
        List.map (fun x -> match x with
            |Positive -> "b"
            |Negative -> "w"
            |Unknown -> ".") k
    String.concat "" listChar


 