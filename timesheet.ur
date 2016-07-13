table user_table: {ID: int, NAME: string} PRIMARY KEY ID

table project_table: {ID: int, NAME: string, DESCRIPTION: string, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user_table(ID)

table task_table: {ID: int, NAME: string, DESCRIPTION: string, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user_table(ID)

table project_task_table: {PROJECT_ID: int, TASK_ID: int} PRIMARY KEY (PROJECT_ID, TASK_ID),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project_table (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES task_table (ID)

table entry_table: {PROJECT_ID: int, TASK_ID: int, DATE: time, TIME: float} PRIMARY KEY (PROJECT_ID, TASK_ID, DATE),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project_table (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES task_table (ID)

datatype entry_cell = EntryCell of time * option float

datatype task_row = TaskRow of int * string * list entry_cell

datatype project_row = ProjectRow of int * string * list task_row

datatype time_sheet = TimeSheet of list time * list project_row

fun intRange i j =
    if i = j
    then []
    else i :: (intRange (i + 1) j)

fun addDays days time =
    Datetime.toTime (Datetime.addDays days (Datetime.fromTime time))

fun timeRange count start = 
    List.mp (fn days => addDays days start) (intRange 0 count)

fun midnight time =
    let val t = Datetime.fromTime time in
	Datetime.toTime {Year = t.Year,
			 Month = t.Month,
			 Day = t.Day,
			 Hour = 0,
			 Minute = 0,
			 Second = 0
			}
    end

fun timeSheet userId count startTime =
    let val startTime = midnight startTime
	val endTime = addDays (count + 1) startTime
	val dates = timeRange count startTime
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
			  WHERE P.USER_ID = {[userId]})
			 (fn entry entries =>
			     return (entry :: entries))
			 [];

	projectIdTaskRowsPairs <- query (SELECT
					   P.ID AS PROJECT_ID,
					   T.ID AS ID,
					   T.NAME AS NAME
					 FROM project_table AS P
					   INNER JOIN project_task_table AS PT ON PT.PROJECT_ID = P.ID
					   INNER JOIN task_table AS T ON T.ID = PT.TASK_ID
					 WHERE P.USER_ID = {[userId]}
					 ORDER BY T.NAME DESC)
					(fn r projectIdTaskRowPairs =>
					    let val entryCells = List.mp (fn date =>
									     case List.find (fn entry =>
												if entry.PROJECT_ID = r.PROJECT_ID
												then if entry.TASK_ID = r.ID
												     then entry.DATE = date
												     else False
												else False)
											    entries of
										 None => EntryCell (date, None)
									       | Some entry => EntryCell (date, Some entry.TIME))
									 dates
						val taskRow = TaskRow (r.ID, r.NAME, entryCells)
						val projectIdTaskRowPair = (r.PROJECT_ID, taskRow)
					    in
						return (projectIdTaskRowPair :: projectIdTaskRowPairs)
					    end)
					[];

	projectRows <- query (SELECT
				P.ID AS ID,
				P.NAME AS NAME
			      FROM project_table AS P
			      WHERE P.USER_ID = {[userId]}
			      ORDER BY P.NAME DESC)
			     (fn r projectRows =>
				 let val projectIdTaskRowsPairs = List.filter (fn (projectId, _) => projectId = r.ID) projectIdTaskRowsPairs
				     val taskRows = List.mp (fn (_, taskRow) => taskRow) projectIdTaskRowsPairs
				     val projectRow = ProjectRow (r.ID, r.NAME, taskRows)
				 in
				     return (projectRow :: projectRows)
				 end)
			     [];

	return (TimeSheet (dates, projectRows))
    end



style container
style row

style col_sm_1
style col_sm_2
style col_sm_3
style col_sm_4
style col_sm_5
style col_sm_6
style col_sm_7
style col_sm_8
style col_sm_9
style col_sm_10
style col_sm_11
style col_sm_12

style glyphicon
style glyphicon_minus_sign
style glyphicon_plus_sign

style css_table
style table_bordered
style table_condensed
style table_responsive
style table_stripped

fun timeSheetPage start = 
    start <- (case start of
		  None => now
		| Some start => return start);

    timeSheet <- timeSheet 1 7 start;

    case timeSheet of
	TimeSheet (dates, projectRows) =>
	projectRows <- List.mapXM (fn projectRow =>
				      case projectRow of
					  ProjectRow (projectId, projectName, taskRows) =>
					  List.mapXiM (fn index taskRow =>
							  case taskRow of
							      TaskRow (taskId, taskName, entryCells) =>
							      entryCells <- List.mapXM (fn entry =>
											   case entry of
											       EntryCell (date, time) =>
											       time <- source (show time);
											       return
												   <xml>
												     <td>
<ctextbox source={time} onkeyup={fn e => if e.KeyCode = 13
					 then time <- get time; rpc (saveEntryCell projectId taskId date time)
					 else return ()}/>
												     </td>
												   </xml>)
										       entryCells;
							      return
								  <xml>
								    <tr>
								      {if index = 0
								       then <xml><td rowspan={List.length taskRows}>{[projectName]}</td></xml>
								       else <xml></xml>}
								      <td>
									{[taskName]}
								      </td>
								      {entryCells}
								    </tr>
								  </xml>)
						      taskRows)
				  projectRows;
	return
	    <xml>
	      <head>
		<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"/>
		<link rel="stylesheet" type="text/css" href="/Timesheet/timesheet.css"/>		
	      </head>
	      <body>
		<div class="container">
		  <div class="row">
		    <div class="col-sm-12">
		      <table class="css_table table_bordered table_condensed table_responsive table_stripped">
			<thead>
			  <tr>
			    <th rowspan=2>Project</th>
			    <th rowspan=2>Task</th>
			    <th colspan={List.length dates}>
			      <a class="glyphicon glyphicon_minus_sign"
			      link={timeSheetPage (Some (addDays (0 - (List.length dates)) start))}></a>
			      
			      Date
			      
			      <a class="glyphicon glyphicon_plus_sign"
			      link={timeSheetPage (Some (addDays (List.length dates) start))}></a>
			    </th>
			  </tr>
			  <tr>
			    {List.mapX (fn date =>
					   <xml>
					     <th>
					       {[timef "%D" date]}
					     </th>
					   </xml>)
				       dates}
			  </tr>
			</thead>
			<tbody>
			  {projectRows}
			</tbody>
		      </table>
		    </div>
		  </div>
		</div>		
	      </body>
	    </xml>

and saveEntryCell projectId taskId date time =
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
	     SET
	       TIME = {[readError time]}
	     WHERE PROJECT_ID = {[projectId]}
	       AND TASK_ID = {[taskId]}
	       AND DATE = {[date]})

fun main () =
    timeSheetPage None
