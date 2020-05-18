# LAbS - a Language with Attribute-based Stigmergies

This repository contains source code for the LAbS code generator,
which are used by the [SLiVER tool](github.com/labs-lang/sliver) to verify
LAbS systems.

* `LabsCore`: basic data types and function to describe and  manipulate LAbS syntax trees.
* `LabsParser`: a parser for `.labs` files.
* `Frontend`: static checks and intermediate representation for  LAbS systems.
* `LabsTranslate`: the code generator itself.

The included `Makefile` creates a full distribution, containing SLiVER,
LabsTranslate, and a set of example files. 


## Technologies used

* [FParsec](https://www.quanttec.com/fparsec/) (parser combinators) for `LabsParser`
* [Argu](http://fsprojects.github.io/Argu/) (argument parser) for the `LabsTranslate` CLI.
* [DotLiquid](http://dotliquidmarkup.org/) for code generation.

## Build requirements

Building `LabsTranslate` requires `dotnet` v3 or higher.
The included `Makefile` targets either x64 Linux or MacOs (version 10.12 "Sierra" and higher).

## Build instruction

```
git clone <this repository> labs/
cd labs/
make [osx|linux]
```

The executables will be within `labs/build/`.

## Publications


[1] R. De Nicola, L. Di Stefano, and O. Inverso, “Multi-Agent Systems with Virtual Stigmergy,” in: Mazzara M., Ober I., Salaün G. (eds) Software Technologies: Applications and Foundations. STAF 2018. Lecture Notes in Computer Science, vol 11176. Springer, 2018. [Link](https://link.springer.com/chapter/10.1007%2F978-3-030-04771-9_26)

[2] R. De Nicola, L. Di Stefano, and O. Inverso, “Multi-Agent Systems with Virtual Stigmergy,” Science of Computer Programming 187, 2020. [Link](https://doi.org/10.1016/j.scico.2019.102345)

[3] L. Di Stefano, F. Lang, and W. Serwe, “Combining SLiVER with CADP to Analyze Multi-agent Systems”, in COORDINATION, 2020. To appear. [Link](http://www.discotec.org/2020/coordination.html)
