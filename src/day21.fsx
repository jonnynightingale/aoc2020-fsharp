module Day21 =

    [<Struct>]
    type IngredientsList = {
        Ingredients : string array
        Allergens : string array
    }

    let parseIngredientsList (line : string) =
        let splitLine = line.Split '('
        {
            Ingredients = splitLine.[0].[.. splitLine.[0].Length - 2].Split ' '
            Allergens = splitLine.[1].[9 .. splitLine.[1].Length - 2].Split ", "
        }

    let getPossibleHostIngredients ingredientsLists allIngredients allergen =
        let ingredientsListsContainingAllergen =
            ingredientsLists
            |> Array.filter (fun il -> il.Allergens |> Array.contains allergen)
            |> Array.map (fun il -> il.Ingredients)
        allIngredients |> Array.filter(fun i -> ingredientsListsContainingAllergen |> Array.forall (Array.contains i))

    let refinePossibleHostIngredientsList allergensAndPossibleIngredients =
        let rec impl refined workingList =
            let nextIndexOpt = workingList |> Array.tryFindIndex (snd >> Array.length >> (=) 1)
            match nextIndexOpt with
            | None -> refined
            | Some i ->
                let allergen = workingList.[i] |> fst
                let ingredient = workingList.[i] |> snd |> Array.exactlyOne
                let listWithIngredientRemoved =
                    workingList
                    |> Array.map (fun (a, is) -> a, is |> Array.filter ((<>) ingredient))
                impl ((allergen, ingredient) :: refined) listWithIngredientRemoved

        allergensAndPossibleIngredients |> impl [] |> List.toArray

    let findAllergenIngredientPairs ingredientsLists allIngredients allAllergens =
        allAllergens
        |> Array.map (fun a -> a, getPossibleHostIngredients ingredientsLists allIngredients a)
        |> refinePossibleHostIngredientsList

    let countOccurencesOfNonAllergenIngredients allergenIngredients ingredientsLists =
        ingredientsLists
        |> Array.map (fun il ->
            il.Ingredients
            |> Array.filter (fun i ->
                allergenIngredients |> Array.map snd |> Array.contains i |> not
            )
        )
        |> Array.sumBy Array.length

    let getCanonicalDangerousIngredientList allergenIngredients =
        let sortedIngredients : string array = allergenIngredients |> Array.sortBy fst |> Array.map snd
        System.String.Join (',', sortedIngredients)

    let solve input =
        let ingredientsLists = input |> Array.map parseIngredientsList
        let allIngredients = ingredientsLists |> Array.collect (fun il -> il.Ingredients) |> Array.distinct
        let allAllergens = ingredientsLists |> Array.collect (fun il -> il.Allergens) |> Array.distinct
        let allergenIngredients = findAllergenIngredientPairs ingredientsLists allIngredients allAllergens
        let partOne = countOccurencesOfNonAllergenIngredients allergenIngredients ingredientsLists
        let partTwo = allergenIngredients |> getCanonicalDangerousIngredientList
        uint64 partOne, partTwo

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day21.solve
printfn "Day 21: [ %i, %s ]" (fst solution) (snd solution)
