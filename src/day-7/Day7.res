module Input = {
  let data =
    Node.Fs.readFileAsUtf8Sync("./input.txt")
    |> Js.String.split(",")
    |> Array.map(x => x->int_of_string)

  let testData = "16,1,2,0,4,2,7,1,2,14" |> Js.String.split(",") |> Array.map(x => x->int_of_string)
}

open Belt
open Input

let add = (a, b) => a + b
let getDiff = (a, b) => Js.Math.abs_int(a - b)

module Part1 = {
  let make = () => {
    let calc = (accumulator, current) =>
      accumulator->Array.concat([data->Array.reduce(0, (a, c) => c->getDiff(current)->add(a))])

    data->Array.reduce([], calc)->SortArray.Int.stableSort->Array.getUnsafe(0)
  }
}

module Part2 = {
  let make = () => {
    let calc = (accumulator, current) =>
      accumulator->Array.concat([
        data->Array.reduce(0, (a, c) =>
          a + Array.range(0, getDiff(c, current))->Array.reduce(0, add)
        ),
      ])

    data->Array.reduce([], calc)->SortArray.Int.stableSort->Array.getUnsafe(0)
  }
}

Part1.make() |> Js.log2("Answer: ")
Part2.make() |> Js.log2("Answer: ")
