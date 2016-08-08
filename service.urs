type entryCell = time * option float
type taskRow = int * string * bool * list entryCell
type projectRow = int * string * bool * list taskRow
type timeSheet = list time * list projectRow

val addDays: time -> int -> time
val timeSheet: int -> time -> int -> transaction timeSheet
val saveEntryCell: int -> int -> time -> float -> transaction unit
val saveProjectVisibility: int -> bool -> transaction unit
val saveTaskVisibility: int -> int -> bool -> transaction unit
