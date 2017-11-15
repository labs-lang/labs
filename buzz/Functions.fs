namespace Buzz
    module Functions = 
        let d (p1: Point, p2: Point) =
            (float ((fst p1) - (fst p2)))**2.0 + (float ((snd p1) - (snd p2)))**2.0
            |> sqrt
        let timeof ( tv: Tval ) = snd tv

        let initLoc p = Map.empty<string, AttrVal>.Add("loc", p)
