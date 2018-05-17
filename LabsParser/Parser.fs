module Parser

open FParsec
open Types
open Common
open Processes
open Components
open System

let parse = 
    manyComments 
    >>. tuple3 (processes) (ws (pMap (many pcomp))) psys
    .>> manyComments
    |>> (fun (procs, comps, (env, spawn)) -> 
        {
        processes = procs;
        components = comps;
        environment = env;
        spawn = spawn
        })