module Input = {
  let data = Node.Fs.readFileAsUtf8Sync("./input.txt") |> Js.String.split("\n")

  let testData = "forward 5
down 5
forward 8
up 3
down 8
forward 2" |> Js.String.split("\n")
}

open Belt

module Part1 = {
  let count = (x, cmd) => {
    switch cmd->Js.String2.split(" ") {
    | ["down", n] => x + int_of_string(n)
    | ["up", n] => x - int_of_string(n)
    | _ => x
    }
  }

  let make = () => {
    let data = Input.data->Array.partition(x => x->Js.String2.includes("forward"))

    data->snd->Array.reduce(0, count) *
      data
      ->fst
      ->Array.reduce(0, (x, y) => x + y->Js.String2.split(" ")->Array.getUnsafe(1)->int_of_string)
  }
}

module Part2 = {
  let count = ((aim, hp, d), cmd) => {
    switch cmd->Js.String2.split(" ") {
    | ["down", n] => (aim + int_of_string(n), hp, d)
    | ["up", n] => (aim - int_of_string(n), hp, d)
    | ["forward", n] => (aim, hp + int_of_string(n), d + aim * int_of_string(n))
    | _ => (aim, hp, d)
    }
  }

  let make = () => {
    let (_, hp, d) = Input.data->Array.reduce((0, 0, 0), count)
    hp * d
  }
}

Part1.make() |> Js.log2("Answer: ")
Part2.make() |> Js.log2("Answer: ")
