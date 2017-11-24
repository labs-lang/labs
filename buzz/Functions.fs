namespace Buzz
    module Functions = 
        let d (p1: Point, p2: Point) =
            (float ((fst p1) - (fst p2)))**2.0 + (float ((snd p1) - (snd p2)))**2.0
            |> sqrt
        let timeof ( tv: Tval ) = snd tv

        let initLoc p = Map.empty<string, Val>.Add("loc", p)

        let link l1 l2 =
            let DELTA = 2.0

            match (l1, l2) with
            | (Int(a), Int(b)) -> (float >> abs) (a - b) <= DELTA
            | (P(p1), P(p2)) -> d(p1, p2) <= DELTA
            | _ -> false
        
        let rec torusProj (xMax:int) (yMax:int) (p:Point) =
            let tP = torusProj xMax yMax
            match p with
            | (x, y) when x<0 -> tP (xMax, y)
            | (x, y) when y<0 -> tP (x, yMax)
            | (x, y) when x>xMax -> tP (0, y)
            | (x, y) when y>yMax -> tP (x, 0)
            | _ -> p