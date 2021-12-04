module Input = {
  let data = Node.Fs.readFileAsUtf8Sync("./input.txt") |> Js.String.split("\n")

  let testData = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010" |> Js.String.split("\n")
}

open Belt
open Input

let isZero = x => x === "0"
let getGamma = ((a, b)) => a->Array.size > b->Array.size ? 0 : 1
let getEpsilon = ((a, b)) => a->Array.size < b->Array.size ? 0 : 1

module Part1 = {
  let make = () => {
    let gamma =
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
      ->Array.map(y => data->Array.map(x => x->Js.String.get(y)))
      ->Array.map(x => x->Array.partition(isZero))
      ->Array.map(getGamma)
      ->Array.joinWith("", x => string_of_int(x))

    let epsilon =
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
      ->Array.map(y => data->Array.map(x => x->Js.String.get(y)))
      ->Array.map(x => x->Array.partition(isZero))
      ->Array.map(getEpsilon)
      ->Array.joinWith("", x => string_of_int(x))

    (("0b"++gamma)-> Int32.of_string, ("0b"++epsilon)-> Int32.of_string)
  }
}

module Part2 = {
  let getVerticalByIndex = (list, index) => list->Array.map(x => x->Js.String.get(index))

  let make = () => {
    let rec fun = (data, byIndex, deriveFirstLetter) => {
      switch data->Array.size {
      | 1 => data->Array.getUnsafe(0)
      | _ =>
        data
        ->Array.keep(dataLine => {
          let (zeros, ones) = getVerticalByIndex(data, byIndex)->Array.partition(isZero)
          let l = deriveFirstLetter(zeros, ones)

          dataLine->Js.String.get(byIndex) === l
        })
        ->fun(byIndex + 1, deriveFirstLetter)
      }
    }

    let hasMoreZeros = ((zeros, ones)) => zeros->Array.size > ones->Array.size
    let deriveLetter = (val, fn) => fn(val) ? "1" : "0"

    let a = "0b"++fun(data, 0, (a, b) => deriveLetter((a,b), (a) => !hasMoreZeros(a)))
    let b = "0b"++fun(data, 0, (a, b) => deriveLetter((a,b), hasMoreZeros))

    (a->Int32.of_string, b->Int32.of_string)
  }
}



Part2.make() |> Js.log2("Answer: ")
Part1.make() |> Js.log2("Answer: ")
