namespace Shared

open System



type HashSet<'T when 'T: equality and 'T :> obj>(size: int) =
    member val Size = size
    member val private elements = Array.create size Option<'T>.None

    member private this.hash(item: 'T) = (item.GetHashCode() |> Math.Abs) % size

    member private this.findPosition element =
        let hash = this.hash element

        let rec iterate iter next =
            if (iter < this.elements.Length) then
                match iter, this.elements.[iter] with
                | id, Some x when id = hash && x = element -> Some id // если первый попавшийся элемент совпадает с искомым
                | id, Some _ when id <> hash -> iterate next ((next + 1) % size) // если произошла колизия
                | id, None when id <> hash -> Some id // если после колизии найдено свободное место
                | _ -> None
            else
                None

        iterate hash (hash + 1)

    member this.add(element: 'T) =
        this.findPosition element
        |> function
            | Some h -> this.elements.[h] <- Some element
            | None -> ()

    member this.has(element: 'T) =
        let initial = this.hash element

        let rec iterate item hash =
            this.elements.[hash]
            |> function
                | Some current when current = item -> Some current
                | Some current when current <> item -> iterate item ((hash + 1) % size)
                | _ when hash = initial -> None
                | _ -> None

        iterate element initial

    member this.Print() =
        let rec iterate (arr: 'T option array) i emptyCount =
            if (i < arr.Length) then
                match i, arr.[i], emptyCount with
                | i, Some x, _ ->
                    printfn $"[{i}]: {x}"
                    (i + 1, 0)
                | 0, None, _ ->
                    printfn $"[{i}]: None"
                    (i + 1, 1)
                | i, None, 2 ->
                    printfn "..."
                    (i + 1, emptyCount + 1)
                | _ -> (i + 1, emptyCount + 1)
                |> fun (index, empty) -> iterate arr index empty

        iterate this.elements 0 0
