(* Service *)
type projectRow = string * bool
type timeSheet = (list projectRow)

fun timeSheet (): transaction timeSheet =
    let val p1 = ("Project 1", True)
	val p2 = ("Project 2", True)
	val p3 = ("Project 3", True)
	val p4 = ("Project 4", True)
	val p5 = ("Project 5", True)
	val p6 = ("Project 6", True)
	val p7 = ("Project 7", True)
	val p8 = ("Project 8", True)
	val p9 = ("Project 9", True)
	val p10 = ("Project 10", True)
    in
	return (
	Cons(p1,
	     Cons(p2,
		  Cons(p3,
		       Cons(p4,
			    Cons(p5,
				 Cons(p6,
				      Cons(p7,
					   Cons(p8,
						Cons(p9,
						     Cons(p10, Nil)))))))))))
    end



(* Model *)
type projectRowModel = string *
		       (source bool) *
		       (mouseEvent -> transaction unit)

fun projectRowModel ((projectName, isProjectRowVisible): projectRow): transaction projectRowModel =
    isProjectRowVisibleSource <- source isProjectRowVisible;    
    let fun toggleProjectRowVisibility _ =
	    isProjectRowVisible <- get isProjectRowVisibleSource;
	    set isProjectRowVisibleSource (not isProjectRowVisible)
    in
	return (projectName, isProjectRowVisibleSource, toggleProjectRowVisibility)
    end

type timeSheetModel = (source (option ((list projectRowModel) * 
				       (source bool) *
				       (source bool) *
				       (mouseEvent -> transaction unit)))) * 
		      (unit -> transaction unit)

fun timeSheetModelOptionSource (): transaction timeSheetModel =
    timeSheetModelOptionSource <- source None;
    let fun load () =
	    trueSource <- source True;
	    isPinningSource <- source False;
	    let fun togglePinning _ =
		    isPinning <- get isPinningSource;
		    set isPinningSource (not isPinning)
	    in
		timeSheet <- timeSheet ();
		case timeSheet of
		    (projectRows) =>
		    projectRows <- List.mapM projectRowModel projectRows;
		    set timeSheetModelOptionSource (Some (projectRows, trueSource, isPinningSource, togglePinning))
	    end
    in
	return (timeSheetModelOptionSource, load)
    end



(* View *)
fun pushPinButtonView isVisibleSource isActiveSource onclick =
    isVisible <- signal isVisibleSource;
    isActive <- signal isActiveSource;
    if isVisible then
	return
	    <xml>
	      <button onclick={onclick}>
		{if isActive
		 then cdata "Pressed"
		 else cdata "Released"}
	      </button>
	    </xml>
    else
	return <xml></xml>

fun projectRowView isPinningSource (projectName, isProjectRowVisibleSource, toggleProjectRowVisibility) =
    isPinning <- signal isPinningSource;
    isProjectRowVisible <- signal isProjectRowVisibleSource;
    if isPinning || isProjectRowVisible then
	return 
	    <xml>
	      <tr>
		<th>
		  {[projectName]}		  
		  <dyn signal={pushPinButtonView isPinningSource isProjectRowVisibleSource toggleProjectRowVisibility}/>
		</th>
	      </tr>
	    </xml>
    else
        return <xml></xml>

fun timeSheetView timeSheetModelOptionSource =
    timeSheetModelOption <- signal timeSheetModelOptionSource;

    case timeSheetModelOption of
	Some (projectRows, trueSource, isPinningSource, togglePinning) =>
	return
	    <xml>
	      <table>
		<thead>
		  <tr>
		    <th>
		      <dyn signal={pushPinButtonView trueSource isPinningSource togglePinning}/>
		    </th>
		  </tr>
		</thead>		
		<tbody>
		  {List.mapX (fn projectRow => <xml><dyn signal={projectRowView isPinningSource projectRow}/></xml>) projectRows}
		</tbody>
	      </table>
	    </xml>
      | None =>
	return <xml></xml>



(* Application *)
fun main () =
    timeSheetModelOptionSource <- timeSheetModelOptionSource ();

    case timeSheetModelOptionSource of
	(timeSheetModelOptionSource, load) =>
	return
	    <xml>
	      <body onload={load ()}>
		<dyn signal={timeSheetView timeSheetModelOptionSource}/>
	      </body>
	    </xml>
