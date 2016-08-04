(* Service *)
table user_table: {ID: int, NAME: string} PRIMARY KEY ID

table project_table: {ID: int, NAME: string, DESCRIPTION: string, VISIBLE: bool, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user_table(ID)

table task_table: {ID: int, NAME: string, DESCRIPTION: string, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user_table(ID)

table project_task_table: {PROJECT_ID: int, TASK_ID: int, VISIBLE: bool} PRIMARY KEY (PROJECT_ID, TASK_ID),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project_table (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES task_table (ID)

table entry_table: {PROJECT_ID: int, TASK_ID: int, DATE: time, TIME: float} PRIMARY KEY (PROJECT_ID, TASK_ID, DATE),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project_table (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES task_table (ID)

fun midnight (time: time): time =
    let val t = Datetime.fromTime time in
	Datetime.toTime {Year = t.Year, Month = t.Month, Day = t.Day, Hour = 0, Minute = 0, Second = 0}
    end

fun addDays (time: time) (days: int): time =
    Datetime.toTime (Datetime.addDays days (Datetime.fromTime time))

fun intRange (i: int) (j: int): list int =
    if i >= j
    then []
    else i :: (intRange (i + 1) j)

fun timeRange (start: time) (count: int): list time =
    List.mp (fn days => addDays start days) (intRange 0 count)

type entryCell = time * (option float)
type taskRow = int * string * bool * (list entryCell)
type projectRow = int * string * bool * (list taskRow)
type timeSheet = (list time) * (list projectRow)

fun timeSheet (userId: int) (startTime: time) (count: int): transaction timeSheet =
    let val startTime = midnight startTime
	val endTime = addDays startTime (count + 1)
	val dates = timeRange startTime count
    in
	entries <- query (SELECT
			    E.PROJECT_ID AS PROJECT_ID,
			    E.TASK_ID AS TASK_ID,
			    E.DATE AS DATE,
			    E.TIME AS TIME
			  FROM project_table AS P
			    INNER JOIN project_task_table AS PT ON PT.PROJECT_ID = P.ID
			    INNER JOIN task_table AS T ON T.ID = PT.TASK_ID
			    INNER JOIN entry_table AS E ON E.PROJECT_ID = PT.PROJECT_ID AND E.TASK_ID = PT.TASK_ID
			  WHERE P.USER_ID = {[userId]}
			    AND {[startTime]} <= E.DATE
			    AND E.DATE <= {[endTime]})
			 (fn entry entries => return (entry :: entries))
			 [];

	projectIdTaskRowsPairs <- query (SELECT
					   P.ID AS PROJECT_ID,
					   T.ID AS ID,
					   T.NAME AS NAME,
					   PT.VISIBLE AS VISIBLE
					 FROM project_table AS P
					   INNER JOIN project_task_table AS PT ON PT.PROJECT_ID = P.ID
					   INNER JOIN task_table AS T ON T.ID = PT.TASK_ID
					 WHERE P.USER_ID = {[userId]}
					 ORDER BY T.NAME DESC)
					(fn r projectIdTaskRowPairs =>
					    let val entryCells = List.mp (fn date => case List.find (fn entry => entry.PROJECT_ID = r.PROJECT_ID &&
														 entry.TASK_ID = r.ID &&
														 entry.DATE = date) entries of
											 None => (date, None)
										       | Some entry => (date, Some entry.TIME))
									 dates
						val taskRow = (r.ID, r.NAME, r.VISIBLE, entryCells)
						val projectIdTaskRowPair = (r.PROJECT_ID, taskRow)
					    in
						return (projectIdTaskRowPair :: projectIdTaskRowPairs)
					    end)
					[];

	projectRows <- query (SELECT
				P.ID AS ID,
				P.NAME AS NAME,
				P.VISIBLE AS VISIBLE
			      FROM project_table AS P
			      WHERE P.USER_ID = {[userId]}
			      ORDER BY P.NAME DESC)
			     (fn r projectRows =>
				 let val projectIdTaskRowsPairs = List.filter (fn (projectId, _) => projectId = r.ID) projectIdTaskRowsPairs
				     val taskRows = List.mp (fn (_, taskRow) => taskRow) projectIdTaskRowsPairs
				     val projectRow = (r.ID, r.NAME, r.VISIBLE, taskRows)
				 in
				     return (projectRow :: projectRows)
				 end)
			     [];

	return (dates, projectRows)
    end

fun saveEntryCell (projectId: int) (taskId: int) (date: time) (time: string) =
    count <- oneRowE1(SELECT COUNT( * )
		      FROM entry_table AS E
		      WHERE E.PROJECT_ID = {[projectId]}
			AND E.TASK_ID = {[taskId]}
			AND E.DATE = {[date]});

    if count = 0 then
	dml (INSERT INTO entry_table (PROJECT_ID, TASK_ID, DATE, TIME)
	     VALUES ({[projectId]}, {[taskId]}, {[date]}, {[readError time]}))
    else
	dml (UPDATE entry_table
	     SET TIME = {[readError time]}
	     WHERE PROJECT_ID = {[projectId]}
	       AND TASK_ID = {[taskId]}
	       AND DATE = {[date]})

fun saveProjectVisibility (id: int) (visible: bool): transaction unit =
    dml (UPDATE project_table
	 SET VISIBLE = {[visible]}
	 WHERE ID = {[id]})

fun saveTaskVisibility (projectId: int) (taskId: int) (visible: bool): transaction unit =
    dml (UPDATE project_task_table
	 SET VISIBLE = {[visible]}
	 WHERE PROJECT_ID = {[projectId]}
	   AND TASK_ID = {[taskId]})



(* Model *)
ffi blur: id -> transaction unit

type entryCellModel = id *
		      (source string) *
		      (keyEvent -> transaction unit)

fun entryCellModel (projectId: int) (taskId: int) ((date, time): entryCell): transaction entryCellModel =
    id <- fresh;    
    timeSource <- source (show time);    
    let fun onkeyup event = if event.KeyCode = 13 then
				time <- get timeSource;
				rpc (saveEntryCell projectId taskId date time);
				blur id
			    else
				return ()
    in
	return (id, timeSource, onkeyup)
    end

type taskRowModel = int *
		    string *
		    source bool *
		    (list entryCellModel) *
		    (_ -> transaction unit)

fun taskRowModel (projectId: int) ((taskId, name, isVisible, entryCells): taskRow): transaction taskRowModel =
    isVisibleSource <- source isVisible;
    entryCells <- List.mapM (fn entryCell => entryCellModel projectId taskId entryCell) entryCells;
    let fun toggleVisibility _ =
	    isVisible <- get isVisibleSource;
	    set isVisibleSource (not isVisible)
    in
	return (taskId, name, isVisibleSource, entryCells, toggleVisibility)
    end

type projectRowModel = int *
		       string *
		       (source bool) *
		       (list taskRowModel) *
		       (_ -> transaction unit)

fun projectRowModel ((projectId, projectName, isProjectRowVisible, taskRows): projectRow): transaction projectRowModel =
    isProjectRowVisibleSource <- source isProjectRowVisible;    
    taskRows <- List.mapM (fn taskRow => taskRowModel projectId taskRow) taskRows;
    let fun toggleProjectRowVisibility _ =
	    isProjectRowVisible <- get isProjectRowVisibleSource;
	    set isProjectRowVisibleSource (not isProjectRowVisible)
    in
	return (projectId, projectName, isProjectRowVisibleSource, taskRows, toggleProjectRowVisibility)
    end

type timeSheetModel = (source (option (int *
				       (list time) *
				       (list projectRowModel) * 
				       (source bool) *
				       (source bool) *
				       bool *
				       (_ -> transaction unit) *
				       (_ -> transaction unit) *
				       (_ -> transaction unit) *
				       (_ -> transaction unit) *
				       (_ -> transaction unit)))) *
		      (time -> int -> transaction unit)

fun timeSheetModelOptionSource (userId: int): transaction timeSheetModel =
    timeSheetModelOptionSource <- source None;
    let fun load start count =
	    let fun previous _ = load (addDays start (0 - count)) count
		fun next _ = load (addDays start count) count
		fun minus _ = load start (count - 1)
		fun plus _ = load start (count + 1)
		val isMinusVisible = count > 1
	    in
		trueSource <- source True;
		isPinningSource <- source False;
		let fun togglePinning _ =
			isPinning <- get isPinningSource;
			set isPinningSource (not isPinning)
		in
		    timeSheet <- rpc (timeSheet userId start count);
		    case timeSheet of
			(dates, projectRows) =>
			projectRows <- List.mapM projectRowModel projectRows;
			set timeSheetModelOptionSource (Some (count, dates, projectRows, trueSource, isPinningSource, isMinusVisible, togglePinning, previous, next, minus, plus))
		end
	    end
    in
	return (timeSheetModelOptionSource, load)
    end



(* View *)
style container
style row
style col_sm_12

style btn
style btn_default      
style btn_primary
style btn_sm      

style pull_right

style glyphicon
style glyphicon_chevron_left
style glyphicon_chevron_right
style glyphicon_minus_sign
style glyphicon_plus_sign
style glyphicon_pushpin

style css_table
style table_bordered
style table_condensed
style table_responsive
style table_stripped

fun pushPinButtonView isVisibleSource isActiveSource onclick =
    let val dynClass =
	    isActive <- signal isActiveSource;
	    return (classes (CLASS "btn btn_sm pull_right")
			    (if isActive then btn_primary else btn_default))

	val dynStyle =
	    isVisible <- signal isVisibleSource;
	    return (if isVisible
		    then STYLE "display: initial"
		    else STYLE "display: none")
    in
	return
	    <xml>
	      <button dynClass={dynClass} dynStyle={dynStyle} onclick={onclick}>
		<i class="glyphicon glyphicon_pushpin"></i>
	      </button>
	    </xml>
    end

fun projectRowView isPinningSource (projectId, projectName, isProjectRowVisibleSource, taskRows, toggleProjectRowVisibility) =
    isPinning <- signal isPinningSource;
    isProjectRowVisible <- signal isProjectRowVisibleSource;
    taskRows <- (if isPinning then
		     return taskRows
		 else
		     List.filterM (fn taskRow =>
				      case taskRow of
					  (_, _, isTaskRowVisibleSource, _, _) => signal isTaskRowVisibleSource)
				  taskRows);
    if isPinning || isProjectRowVisible then
	return (List.mapXi (fn index (taskId, taskName, isTaskRowVisibleSource, entryCells, toggleTaskRowVisibility) =>
			       <xml>
				 <tr>
				   {if index = 0 then
					<xml>
					  <th rowspan={List.length taskRows}>
					    {[projectName]}

					    <dyn signal={pushPinButtonView isPinningSource isProjectRowVisibleSource toggleProjectRowVisibility}/>
					  </th>
					</xml>
				    else
					<xml></xml>}

				   <th>
				     {[taskName]}

				     <dyn signal={pushPinButtonView isPinningSource isTaskRowVisibleSource toggleTaskRowVisibility}/>
				   </th>

				   {List.mapX (fn (id, timeSource, onkeyup) => <xml><td><ctextbox id={id} source={timeSource} onkeyup={onkeyup}/></td></xml>) entryCells}
				 </tr>
			       </xml>) taskRows)
    else
	return <xml></xml>

fun timeSheetView timeSheetModelOptionSource =
    timeSheetModelOption <- signal timeSheetModelOptionSource;

    case timeSheetModelOption of
	Some (count, dates, projectRows, trueSource, isPinningSource, isMinusVisible, togglePinning, previous, next, minus, plus) =>
	return
	    <xml>
	      <table class="css_table table_bordered table_condensed table_responsive table_stripped">
		<thead>
		  <tr>
		    <th colspan=2>
		      <dyn signal={pushPinButtonView trueSource isPinningSource togglePinning}/>
		    </th>
		    <th colspan={count}>
		      <a class="glyphicon glyphicon_chevron_left" onclick={previous}/>

		      Date

		      <a class="glyphicon glyphicon_chevron_right" onclick={next}/>

		      <span class="pull_right">
			{if isMinusVisible
			 then <xml><a class="glyphicon glyphicon_minus_sign" onclick={minus}/></xml>
			 else <xml></xml>}

			<a class="glyphicon glyphicon_plus_sign" onclick={plus}/>
		      </span>
		    </th>
		  </tr>
		</thead>
		<tbody>
		  <tr>
		    <th>Project</th>
		    <th>Task</th>
		    {List.mapX (fn date => <xml><th>{[timef "%D" date]}</th></xml>) dates}
		  </tr>
		  {List.mapX (fn projectRow => <xml><dyn signal={projectRowView isPinningSource projectRow}/></xml>) projectRows}
		</tbody>
	      </table>
	    </xml>
      | None =>
	return <xml></xml>



(* Application *)
fun main () =
    timeSheetModelOptionSource <- timeSheetModelOptionSource 1;

    case timeSheetModelOptionSource of
	(timeSheetModelOptionSource, load) =>
	return 
	    <xml>
	      <head>
		<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"/>
		<link rel="stylesheet" type="text/css" href="/Timesheet/timesheet.css"/>
	      </head>
	      <body onload={start <- now; load start 7}>
		<div class="container">
		  <div class="row">
		    <div class="col-sm-12">
		      <dyn signal={timeSheetView timeSheetModelOptionSource}/>
		    </div>
		  </div>
		</div>
	      </body>
	    </xml>
