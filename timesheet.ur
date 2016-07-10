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

fun intRange i j = if i = j
		   then []
		   else i :: (intRange (i + 1) j)

fun addDays days time = let val t = Datetime.fromTime time
			    val result = Datetime.addDays days t
			in
			    Datetime.toTime result
			end

fun timeRange count start = let val daysRange = intRange 0 count in
				List.mp (fn days => addDays days start) daysRange
			    end

fun midnight time = let val t = Datetime.fromTime time in
			Datetime.toTime {Year = t.Year,
					 Month = t.Month,
					 Day = t.Day,
					 Hour = 0,
					 Minute = 0,
					 Second = 0
					}
		    end

fun timeSheet userId count startTime =
    let val startMidnight = midnight startTime
	val endMidnight = addDays (count + 1) startMidnight
	val dates = timeRange count startMidnight
    in
	entries <-
	query (SELECT
		 E.PROJECT_ID AS PROJECT_ID,
		 E.TASK_ID AS TASK_ID,
		 E.DATE AS DATE,
		 E.TIME AS TIME
	       FROM project_table AS P
		 INNER JOIN project_task_table AS PT ON PT.PROJECT_ID = P.ID
		 INNER JOIN task_table AS T ON T.ID = PT.TASK_ID
		 INNER JOIN entry_table AS E ON E.PROJECT_ID = PT.PROJECT_ID AND E.TASK_ID = PT.TASK_ID
	       WHERE P.USER_ID = {[userId]})
	      (fn entry entries => return (entry :: entries))
	      [];

	projectIdTaskRowsPairs <-
	query (SELECT
		 P.ID AS PROJECT_ID,
		 T.ID AS ID,
		 T.NAME AS NAME
	       FROM project_table AS P
		 INNER JOIN project_task_table AS PT ON PT.PROJECT_ID = P.ID
		 INNER JOIN task_table AS T ON T.ID = PT.TASK_ID
	       WHERE P.USER_ID = {[userId]}
	       ORDER BY T.NAME DESC)
	      (fn r projectIdTaskRowsPairs =>
		  let val entryCells =
			  List.mp (fn date =>
				      case List.find (fn entry =>
							 if entry.PROJECT_ID = r.PROJECT_ID
							 then if entry.TASK_ID = r.ID
							      then entry.DATE = date
							      else False
							 else False
						     ) entries of
					  None => EntryCell (date, None)
					| Some entry => EntryCell (date, Some entry.TIME))
				  dates
		      val taskRow = TaskRow (r.ID, r.NAME, entryCells)
		      val projectIdTaskRowsPair = (r.PROJECT_ID, taskRow)
		  in
		      return (projectIdTaskRowsPair :: projectIdTaskRowsPairs)
		  end)
	      [];

	projectRows <-
	query (SELECT
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

fun main () =
    start <- now;

    timeSheet <- timeSheet 1 7 start;

    case timeSheet of
	TimeSheet (dates, projectRows) =>
	return
	    <xml>
	      <body>
		<table border=1>
		  <thead>
		    <tr>
		      <th rowspan=2>Project</th>
		      <th rowspan=2>Task</th>
		      <th colspan={List.length dates}>Date</th>
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
		    {List.mapX (fn projectRow =>
				   case projectRow of
				       ProjectRow (projectId, projectName, taskRows) =>
				       List.mapXi (fn index taskRow =>
						      case taskRow of
							  TaskRow (taskId, taskName, entryCells) =>
							  <xml>
							    <tr>
							      {if index = 0
							       then <xml><td rowspan={List.length taskRows}>{[projectName]}</td></xml>
							       else <xml></xml>}
							      <td>{[taskName]}</td>
							      {List.mapX (fn entry =>
									     case entry of
										 EntryCell (date, time) =>
										 <xml>
										   <td>
										     <form>
										       <textbox{#TIME} value={show time}/>
										       <submit action={saveEntryCell projectId taskId date}/>
										     </form>
										   </td>
										 </xml>)
									 entryCells}
							    </tr>
							  </xml>)
						  taskRows)
			       projectRows}
		  </tbody>
		</table>
	      </body>
	    </xml>

and saveEntryCell projectId taskId date f =
    count <- oneRowE1(SELECT COUNT( * )
		      FROM entry_table AS E);

    if count = 0 then
	dml (INSERT INTO entry_table (PROJECT_ID, TASK_ID, DATE, TIME)
	     VALUES ({[projectId]}, {[taskId]}, {[date]}, {[readError f.TIME]}));
	main ()
    else
	dml (UPDATE entry_table
	     SET
	       TIME = {[readError f.TIME]}
	     WHERE PROJECT_ID = {[projectId]}
	       AND TASK_ID = {[taskId]}
	       AND DATE = {[date]});
	main ()
