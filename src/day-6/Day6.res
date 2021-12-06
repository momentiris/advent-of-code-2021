module Input = {
  let data = Node.Fs.readFileAsUtf8Sync("./input.txt")->Js.String2.split(",")

  let testData = "3,4,3,1,2"->Js.String2.split(",")
}

open Belt

let deriveNextNumber = number =>
  switch number {
  | 0 => 6
  | _ => number - 1
  }

module Part1 = {
  let make = () => {
    let d = Input.data->Array.map(x => x->int_of_string)

    Belt.Array.range(1, 80)
    ->Array.reduce(d, (fish, _) => {
      let newFish = fish->Array.keep(x => x === 0)->Array.map(_ => 8)
      let newState = fish->Array.map(deriveNextNumber)

      newState->Array.concat(newFish)
    })
    ->Array.size
  }
}

module Part2 = {
  let make = () => {
    let d = Input.data->Array.map(x => x->int_of_string)

    let numberOfFishPerDayCount = ref([0, 0, 0, 0, 0, 0, 0, 0, 0])

    d->Array.forEach(x =>
      numberOfFishPerDayCount.contents
      ->Array.set(x, numberOfFishPerDayCount.contents->Array.getUnsafe(x) + 1)
      ->ignore
    )

    Belt.Array.range(1, 256)
    ->Array.reduce(numberOfFishPerDayCount.contents, (acc, _curr) => {
      let zeros = acc->Array.getUnsafe(0)
      let ones = acc->Array.getUnsafe(1)
      let twos = acc->Array.getUnsafe(2)
      let threes = acc->Array.getUnsafe(3)
      let fours = acc->Array.getUnsafe(4)
      let fives = acc->Array.getUnsafe(5)
      let sixes = acc->Array.getUnsafe(6)
      let sevens = acc->Array.getUnsafe(7)
      let eights = acc->Array.getUnsafe(8)

      [ones, twos, threes, fours, fives, sixes, zeros + sevens, eights, zeros]
    })
    ->Array.reduce(0, (acc, curr) => acc + curr)
  }
}

Part2.make() |> Js.log2("Answer: ")
// Part2.make() |> Js.log2("Answer: ")
