open List

table user_table: {ID: int, NAME: string} PRIMARY KEY ID

table project_table: {ID: int, NAME: string, DESCRIPTION: string, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user_table(ID)

table task_table: {ID: int, NAME: string, DESCRIPTION: string, USER_ID: int} PRIMARY KEY ID,
      CONSTRAINT USER_ID FOREIGN KEY USER_ID REFERENCES user_table(ID)

table project_task_table: {PROJECT_ID: int, TASK_ID: int} PRIMARY KEY (PROJECT_ID, TASK_ID),
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project_table (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES task_table (ID)

table entry_table: {PROJECT_ID: int, TASK_ID: int, DATE: time, TIME: int} PRIMARY KEY (PROJECT_ID, TASK_ID, DATE)
      CONSTRAINT PROJECT_ID FOREIGN KEY PROJECT_ID REFERENCES project_table (ID),
      CONSTRAINT TASK_ID FOREIGN KEY TASK_ID REFERENCES task_table (ID)

datatype entry_cell = EntryCell of int * float

datatype task_row = TaskRow of int * string * list entry_cell

datatype project_row = ProjectRow of int * string * list task_row

datatype time_sheet = TimeSheet of list project_row

fun timeSheet userId  =
    projectIdTaskRowsPairs <- query (SELECT P.ID AS PROJECT_ID, T.ID AS ID, T.NAME AS NAME
				     FROM project_table AS P
				       INNER JOIN project_task_table AS PT ON PT.PROJECT_ID = P.ID
				       INNER JOIN task_table AS T ON T.ID = PT.TASK_ID
				     WHERE P.USER_ID = {[userId]}
				     ORDER BY T.NAME)
				    (fn r projectIdTaskRowsPairs =>
					let val projectIdTaskRowsPair = (r.PROJECT_ID, TaskRow (r.ID, r.NAME, [])) in
					    return (projectIdTaskRowsPair :: projectIdTaskRowsPairs)
					end)
				    [];

    projectRows <- query (SELECT P.ID AS ID, P.NAME AS NAME
			  FROM project_table AS P
			  WHERE P.USER_ID = {[userId]}
			  ORDER BY P.NAME)
			 (fn r projectRows =>
			     let val projectIdTaskRowsPairs = filter (fn (projectId, _) => projectId = r.ID) projectIdTaskRowsPairs
				 val taskRows = mp (fn (_, taskRow) => taskRow) projectIdTaskRowsPairs
				 val projectRow = ProjectRow (r.ID, r.NAME, taskRows)
			     in
				 return (projectRow :: projectRows)
			     end)
			 [];

    return (TimeSheet projectRows)
    
fun main () =
    timeSheet <- timeSheet 1;

    return
	<xml>
	  <body>
	    <table>
	      {case timeSheet of
		   TimeSheet (projectRows) =>
  		   mapX (fn projectRow =>
			    case projectRow of
				ProjectRow (projectId, projectName, taskRows) =>
				mapX (fn taskRow =>
					 case taskRow of
					     TaskRow (taskId, taskName, _) =>					     
					     <xml>
					       <tr>
						 <td>{[projectId]}</td>
						 <td>{[projectName]}</td>
						 <td>{[taskId]}</td>
						 <td>{[taskName]}</td>						 
					       </tr>
					     </xml>)
				     taskRows)
			projectRows}
	    </table>
	  </body>
	</xml>
