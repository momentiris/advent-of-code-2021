open Belt

module Input = {
  let data =
    Node.Fs.readFileAsUtf8Sync("./input.txt")
    ->Js.String2.split("\n")
    ->Array.map(x => x->Js.String2.split(""))

  let sample =
    "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
"
    ->Js.String2.split("\n")
    ->Array.map(x => x->Js.String2.split(""))

  let rowLen = sample->Array.getUnsafe(0)->Array.size
}

type pos = Left | Right | Neither

let deriveModPos = index => {
  let xmod = mod(index, Input.rowLen)
  switch (xmod === Input.rowLen - 1, xmod === 0) {
  | (true, _) => Right
  | (_, true) => Left
  | _ => Neither
  }
}

let getAdjacent = index => {
  let (top, bottom, left, right, topLeft, topRight, bottomLeft, bottomRight) = (
    index - Input.rowLen,
    index + Input.rowLen,
    index - 1,
    index + 1,
    index - Input.rowLen - 1,
    index - Input.rowLen + 1,
    index + Input.rowLen - 1,
    index + Input.rowLen + 1,
  )

  switch deriveModPos(index) {
  | Right => [top, left, bottom, topLeft, bottomLeft]
  | Left => [top, right, bottom, topRight, bottomRight]
  | Neither => [top, bottom, left, right, topLeft, topRight, bottomLeft, bottomRight]
  }
}

let increase = list => list->Array.map(x => x + 1)
let isWithin = (list, x) => list->Array.getBy(y => y === x)->Option.isSome

let goldenStep = ref(0)

let rec flash = (octos, flashed, numFlashes, g) => {
  if octos->Array.every(x => x === 0) && goldenStep.contents === 0 {
    goldenStep.contents = g
  }
  switch octos->Array.getIndexBy(x => x > 9) {
  | Some(index) =>
    octos
    ->Array.mapWithIndex((i, x) =>
      getAdjacent(index)->Array.getBy(n => n === i)->Option.isSome && !isWithin(flashed, i)
        ? x + 1
        : x
    )
    ->Array.mapWithIndex((i, x) => i === index ? 0 : x)
    ->flash(flashed->Array.concat([index]), numFlashes + 1, g)

  | _ => (
      g + 1,
      numFlashes,
      octos->Array.mapWithIndex((i, x) =>
        flashed->Array.getBy(n => n === i)->Option.isSome ? 0 : x
      ),
    )
  }
}

let step = ((goldenStep, sum, list), _) => list->Array.map(x => x + 1)->flash([], sum, goldenStep)

module Part1 = {
  let make = () => {
    let l = Input.data->Array.concatMany->Array.map(int_of_string)

    Array.reduce(Array.range(0, 99), (0, 0, l), step)
  }
}

module Part2 = {
  let make = () => {
    let l = Input.data->Array.concatMany->Array.map(int_of_string)

    Array.reduce(Array.range(0, 999), (0, 0, l), step)->ignore
    goldenStep.contents + 1
  }
}

Part2.make() |> Js.log2("Answer: ")
