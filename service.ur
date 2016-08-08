table user: {ID: int, NAME: string} PRIMARY KEY ID

table project: {ID: int, NAME: string, DESCRIPTION: string, VISIBLE: bool, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user (ID)

table taskTable: {ID: int, NAME: string, DESCRIPTION: string, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user (ID)

table projectTask: {PROJECT_ID: int, TASK_ID: int, VISIBLE: bool} PRIMARY KEY (PROJECT_ID, TASK_ID),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES taskTable (ID)

table entry: {PROJECT_ID: int, TASK_ID: int, DATE: time, TIME: float} PRIMARY KEY (PROJECT_ID, TASK_ID, DATE),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES taskTable (ID)

fun midnight (time: time): time =
    let val t = Datetime.fromTime time in
	Datetime.toTime {Year = t.Year, Month = t.Month, Day = t.Day, Hour = 0, Minute = 0, Second = 0}
    end

fun addDays (time: time) (days: int): time =
    Datetime.toTime (Datetime.addDays days (Datetime.fromTime time))

fun intRange (i: int) (j: int): list int =
    if i > j
    then []
    else i :: (intRange (i + 1) j)

fun timeRange (start: time) (count: int): list time =
    List.mp (fn days => addDays start days) (intRange 0 count)

fun timeSheet (userId: int) (startTime: time) (count: int): transaction timeSheet =
    let val startTime = midnight startTime
	val endTime = addDays startTime count
	val dates = timeRange startTime (count - 1)
    in
	entries <- query (SELECT
			    E.PROJECT_ID AS PROJECT_ID,
			    E.TASK_ID AS TASK_ID,
			    E.DATE AS DATE,
			    E.TIME AS TIME
			  FROM project AS P
			    INNER JOIN projectTask AS PT ON PT.PROJECT_ID = P.ID
			    INNER JOIN taskTable AS T ON T.ID = PT.TASK_ID
			    INNER JOIN entry AS E ON E.PROJECT_ID = PT.PROJECT_ID AND E.TASK_ID = PT.TASK_ID
			  WHERE P.USER_ID = {[userId]}
			    AND {[startTime]} <= E.DATE
			    AND E.DATE <= {[endTime]})
			 (fn entry entries => return (entry :: entries)) [];

	projectIdTaskRowsPairs <- query (SELECT
					   P.ID AS PROJECT_ID,
					   T.ID AS ID,
					   T.NAME AS NAME,
					   PT.VISIBLE AS VISIBLE
					 FROM project AS P
					   INNER JOIN projectTask AS PT ON PT.PROJECT_ID = P.ID
					   INNER JOIN taskTable AS T ON T.ID = PT.TASK_ID
					 WHERE P.USER_ID = {[userId]}
					 ORDER BY T.NAME DESC)
					(fn r projectIdTaskRowPairs =>
					    let val entryCells = List.mp (fn date => case List.find (fn entry => entry.PROJECT_ID = r.PROJECT_ID &&
														 entry.TASK_ID = r.ID &&
														 entry.DATE = date) entries of
											 None => (date, None)
										       | Some entry => (date, Some entry.TIME)) dates
						val taskRow = (r.ID, r.NAME, r.VISIBLE, entryCells)
						val projectIdTaskRowPair = (r.PROJECT_ID, taskRow)
					    in
						return (projectIdTaskRowPair :: projectIdTaskRowPairs)
					    end) [];

	projectRows <- query (SELECT
				P.ID AS ID,
				P.NAME AS NAME,
				P.VISIBLE AS VISIBLE
			      FROM project AS P
			      WHERE P.USER_ID = {[userId]}
			      ORDER BY P.NAME DESC)
			     (fn r projectRows =>
				 let val projectIdTaskRowsPairs = List.filter (fn (projectId, _) => projectId = r.ID) projectIdTaskRowsPairs
				     val taskRows = List.mp (fn (_, taskRow) => taskRow) projectIdTaskRowsPairs
				     val projectRow = (r.ID, r.NAME, r.VISIBLE, taskRows)
				 in
				     return (projectRow :: projectRows)
				 end) [];

	return (dates, projectRows)
    end

fun saveEntryCell (projectId: int) (taskId: int) (date: time) (time: float): transaction unit =
    count <- oneRowE1(SELECT COUNT( * )
		      FROM entry AS E
		      WHERE E.PROJECT_ID = {[projectId]}
			AND E.TASK_ID = {[taskId]}
			AND E.DATE = {[date]});

    if count = 0 then
	dml (INSERT INTO entry (PROJECT_ID, TASK_ID, DATE, TIME)
	     VALUES ({[projectId]}, {[taskId]}, {[date]}, {[time]}))
    else
	dml (UPDATE entry
	     SET TIME = {[time]}
	     WHERE PROJECT_ID = {[projectId]}
	       AND TASK_ID = {[taskId]}
	       AND DATE = {[date]})

fun saveProjectVisibility (id: int) (visible: bool): transaction unit =
    dml (UPDATE project
	 SET VISIBLE = {[visible]}
	 WHERE ID = {[id]})

fun saveTaskVisibility (projectId: int) (taskId: int) (visible: bool): transaction unit =
    dml (UPDATE projectTask
	 SET VISIBLE = {[visible]}
	 WHERE PROJECT_ID = {[projectId]}
	   AND TASK_ID = {[taskId]})
