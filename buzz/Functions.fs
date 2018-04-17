namespace Buzz
open System

    module Functions = 

        let inline xor a b = (a || b) && not (a && b)

        /// "Correct" modulo operator
        // http://gettingsharper.de/2012/02/28/how-to-implement-a-mathematically-correct-modulus-operator-in-f/
        let inline modulo n m =
            let mod' = n % m
            if sign mod' >= 0 then mod'
            else abs m + mod'

        let d (p1: Point, p2: Point) =
            (float ((fst p1) - (fst p2)))**2.0 + (float ((snd p1) - (snd p2)))**2.0
            |> sqrt
        let timeof ( tv: Tval ) = snd tv

        let initLoc p = Map.empty<string, Val>.Add("loc", p)



        let distanceLink delta (i1:Interface) (i2:Interface) =
            match (i1.["loc"], i2.["loc"]) with
            | (Int(a), Int(b)) -> (float >> abs) (a - b) <= delta
            | (P(p1), P(p2)) -> d(p1, p2) <= delta
            | _ -> false
        
        let rec torusProj (xMax:int) (yMax:int) (p:Point) =
            let tP = torusProj xMax yMax
            match p with
            | (x, y) when x<0 -> tP (xMax, y)
            | (x, y) when y<0 -> tP (x, yMax)
            | (x, y) when x>xMax -> tP (0, y)
            | (x, y) when y>yMax -> tP (x, 0)
            | _ -> p

        /// Helper: choose random element from a Seq
        let pickRandom seq =
            Seq.tryItem ((Seq.length >> rng.Next) seq) seq

        let rec randomDirection () =
            let x, y  = rng.Next(-1, 2), rng.Next(-1, 2)
            if Math.Abs(x) + Math.Abs(y) = 0
            then randomDirection()
            else P(x,y)

