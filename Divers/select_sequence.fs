(*
    Sequence
    ----------------
    Défini une séquence de jeu
*)
module Sequence
// Typage d'une séquence
type sequence = {
    Id : int
    Text : string
    Choice : Proposition
}
// Objet de proposition
and Proposition(instruction_in : string, choice_in : list<string>) =
    let mutable instruction = instruction_in
    let mutable choice = choice_in
    let mutable answer = 0
    // Pose la question
    member this.Ask(?t_c : int, ?t_l : int) = 
        let t_c' = match t_c with Some x -> x | None -> 10
        let t_l' = match t_c with Some x -> x | None -> 1000
        ConsoleManager.w instruction t_c' t_l'
        let rec disp t_c t_l i = function
        | [] -> ConsoleManager.w "" t_c t_l
        | x :: xs -> 
            ConsoleManager.w (((string) i )+".) "+x) t_c t_l
            disp t_c t_l (i+1) xs
        disp t_c' t_l' 1 choice
        // Oh mon dieu... de la programmation impérative
        let mutable check = true
        let mutable selection = 0
        while check do
            ConsoleManager.w "Votre choix?" t_c' t_l'
            selection <- ((int) (System.Console.Read())) - (int '0')
            ConsoleManager.clearBuffer()
            check <- not (selection > 0 && selection <= (List.length choice))
        answer <- selection
    // Donne la réponse fournie a la question
    member this.Answer() = answer
// Affiche une séquence
let diplsay (s : sequence) (t_c : int) (t_l : int) : int =
    ConsoleManager.w s.Text t_c t_l
    s.Choice.Ask()
    s.Choice.Answer()
