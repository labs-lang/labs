namespace Buzz
    module Functions = 
        let d (p1: Point, p2: Point) =
            (float ((fst p1) - (fst p2)))**2.0 + (float ((snd p1) - (snd p2)))**2.0
            |> sqrt
        let timeof ( tv: Tval ) = snd tv

        let initLoc p = Map.empty<string, AttrVal>.Add("loc", p)

        let link(l1: AttrVal, l2: AttrVal) =
            let DELTA = 1.0

            match (l1, l2) with
            | (Int(a), Int(b)) -> (float >> abs) (a - b) <= DELTA
            | (P(p1), P(p2)) -> d(p1, p2) <= DELTA
            | _ -> false