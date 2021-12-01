module Input = {
  let data =
    Node.Fs.readFileAsUtf8Sync("./input.txt") |> Js.String.split("\n") |> Array.map(int_of_string)

  let testData =
    "199
200
208
210
200
207
240
269
260
263"
    |> Js.String.split("\n")
    |> Array.map(int_of_string)
}

let getArraySliceFromIndex = (list, index) => list->Js.Array.slice(~start=index, ~end_=index + 3)

open Belt
let excludeFirstAndKeep = (list, fn) =>
  list->Array.keepWithIndex((x, index) =>
    switch index {
    | 0 => false
    | _ => fn(x, index)
    }
  )

module Part1 = {
  let make = () =>
    excludeFirstAndKeep(Input.data, (m, index) =>
      m > Input.data->Array.getUnsafe(index - 1)
    )->Array.size
}

module Part2 = {
  let make = () =>
    excludeFirstAndKeep(Input.data, (_, index) => {
      switch (
        getArraySliceFromIndex(Input.data, index - 1),
        getArraySliceFromIndex(Input.data, index),
      ) {
      | ([x, y, z], [x2, y2, z2]) => x2 + y2 + z2 > x + y + z
      | _ => false
      }
    })->Array.size
}

Part1.make() |> Js.log2("Answer: ")
Part2.make() |> Js.log2("Answer: ")
