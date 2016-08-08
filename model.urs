type entryCellModel = id *
		      source string *
		      (keyEvent -> transaction unit)

type taskRowModel = int *
		    string *
		    source bool *
		    list entryCellModel *
		    (mouseEvent -> transaction unit)

type projectRowModel = int *
		       string *
		       source bool *		       
		       list taskRowModel *
		       (mouseEvent -> transaction unit)

type timeSheetModel = source (time *
			      int *
			      list time *
			      list projectRowModel) *
		      source bool *
		      source bool *
		      (mouseEvent -> transaction unit) *
		      (mouseEvent -> transaction unit) *
		      (mouseEvent -> transaction unit) *
		      (mouseEvent -> transaction unit) *
		      (mouseEvent -> transaction unit)

val timeSheetModel: int -> time -> int -> transaction timeSheetModel
