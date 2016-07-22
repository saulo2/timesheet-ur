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

fun intRange i j = if i = j
		   then []
		   else i :: (intRange (i + 1) j)

fun addDays days time = Datetime.toTime (Datetime.addDays days (Datetime.fromTime time))

fun timeRange count start = List.mp (fn days => addDays days start) (intRange 0 count)

fun midnight time = let val t = Datetime.fromTime time in
			Datetime.toTime {Year = t.Year, Month = t.Month, Day = t.Day, Hour = 0, Minute = 0, Second = 0}
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
					    let val entryCells = List.mp (fn date =>
									     case List.find (fn entry =>												
												entry.PROJECT_ID = r.PROJECT_ID &&
												entry.TASK_ID = r.ID &&
												entry.DATE = date)
											    entries of
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

fun saveEntryCell projectId taskId date time =
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

fun saveProjectVisibility id visible =
    dml (UPDATE project_table
	 SET VISIBLE = {[visible]}
	 WHERE ID = {[id]})

fun saveTaskVisibility projectId taskId visible =
    dml (UPDATE project_task_table
	 SET VISIBLE = {[visible]}
	 WHERE PROJECT_ID = {[projectId]}
	   AND TASK_ID = {[taskId]})

(* Model *)
fun timeSheetModel userId count start =
    timeSheet <- timeSheet userId count start;

    case timeSheet of
	(dates, projectRows) =>
	isPinningSource <- source False;
	projectRows <- List.mapM (fn projectRow =>
				     case projectRow of
					 (projectId, projectName, isProjectRowVisible, taskRows) =>
					 taskRows <- List.mapM (fn taskRow =>
								  case taskRow of
								      (taskId, taskName, isTaskRowVisible, entryCells) =>
								      entryCells <- List.mapM (fn entryCell =>
											          case entryCell of
												      (date, time) =>
												      timeSource <- source (show time);
												      id <- fresh;												      
												      return (id, date, timeSource))
											      entryCells;
								      isTaskRowVisibleSource <- source isTaskRowVisible;
								      return (taskId, taskName, isTaskRowVisibleSource, entryCells))
							      taskRows;
					 isProjectRowVisibleSource <- source isProjectRowVisible;
					 return (projectId, projectName, isProjectRowVisibleSource, taskRows))
				 projectRows;
	return (dates, isPinningSource, projectRows)



(* View *)
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

style active
      
style btn
style btn_default      
style btn_primary
style btn_sm

style glyphicon
style glyphicon_chevron_left
style glyphicon_chevron_right
style glyphicon_minus_sign
style glyphicon_plus_sign
style glyphicon_pushpin

style pull_right

style css_table
style table_bordered
style table_condensed
style table_responsive
style table_stripped

ffi blur: Basis.id -> transaction {}

fun pushPinButton isVisibleSource isActiveSource clickHandler =
    <xml>
      <dyn signal={isVisible <- signal isVisibleSource;
		   return (if isVisible then
			       <xml>
				 <button dynClass={isActive <- signal isActiveSource;
						   return (classes (CLASS "btn btn_sm pull_right")
								   (if isActive then btn_primary else btn_default))}
						      onclick={fn event => clickHandler event}>
                                   <i class="glyphicon glyphicon_pushpin"></i>
				 </button>		     
			       </xml>
			   else
			       <xml></xml>)}/>
    </xml>

fun timeSheetView userId count start =    
    timeSheetSource <- source None;

    trueSource <- source True;

    return
<xml>
  <head>
    <link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"/>
    <link rel="stylesheet" type="text/css" href="/Timesheet/timesheet.css"/>
  </head>
  <body onload={count <- return (case count of
				     None => 7
				   | Some count => count);
		start <- (case start of
			      None => now
			    | Some start => return start);		
		timeSheet <- rpc (timeSheetModel userId count start);
		set timeSheetSource (Some timeSheet)}>
    <div class="container">
      <div class="row">
	<div class="col-sm-12">
	  <dyn signal={timeSheet <- signal timeSheetSource;
		       case timeSheet of
			   None => return <xml></xml>
			 | Some (dates, isPinningSource, projectRows) =>
			   projectRows <- List.mapXM (fn projectRow =>
							 case projectRow of
							     (projectId, projectName, isProjectRowVisibleSource, taskRows) =>
							     taskRows <- List.filterM (fn taskRow =>
											  case taskRow of
											      (taskId, taskName, isTaskRowVisibleSource, entryCells) =>
											      isPinning <- signal isPinningSource;
											      isTaskRowVisible <- signal isTaskRowVisibleSource;
											      return (isPinning || isTaskRowVisible))
										      taskRows;
							     List.mapXiM (fn index taskRow =>
									     case taskRow of
										 (taskId, taskName, isTaskRowVisibleSource, entryCells) =>
										 entryCells <- List.mapXM (fn entry =>
													      case entry of
														  (id, date, timeSource) =>
														  return
<xml>
  <td>
    <ctextbox id={id} source={timeSource} onkeyup={fn event => if event.KeyCode = 13 then
								   time <- get timeSource;
								   rpc (saveEntryCell projectId taskId date time);
								   blur id
							       else
								   return ()}/>
  </td>
</xml>)
													  entryCells;
										 return
<xml>
  <dyn signal={isPinning <- signal isPinningSource;
	       isProjectRowVisible <- signal isProjectRowVisibleSource;
	       return (if isPinning || isProjectRowVisible then
			   <xml>
			     <tr>
			     {if index = 0 then
				  <xml>
				    <td rowspan={List.length taskRows}>
                                      {[projectName]}
				      {pushPinButton isPinningSource
						     isProjectRowVisibleSource
						     (fn _ =>
							 isProjectRowVisible <- get isProjectRowVisibleSource;
							 let val isProjectRowVisible = not isProjectRowVisible in
							     rpc (saveProjectVisibility projectId isProjectRowVisible);
							     set isProjectRowVisibleSource isProjectRowVisible
							 end)}
				    </td>
				  </xml>
			      else
				  <xml>
				  </xml>}
			        <td>
				  {[taskName]}
				  {pushPinButton isPinningSource
						 isTaskRowVisibleSource
						 (fn _ =>
						     isTaskRowVisible <- get isTaskRowVisibleSource;
						     let val isTaskRowVisible = not isTaskRowVisible in
							 rpc (saveTaskVisibility projectId taskId isTaskRowVisible);
							 set isTaskRowVisibleSource isTaskRowVisible
						     end)}
				</td>
				{entryCells}
			     </tr>
			   </xml>
		       else
			   <xml></xml>)}/>
</xml>)
									 taskRows)
						     projectRows;
			   return
<xml>
  <table class="css_table table_bordered table_condensed table_responsive table_stripped">
    <thead>
      <tr>
	<th colspan=2>
	  {pushPinButton trueSource isPinningSource (fn _ => isPinning <- get isPinningSource; set isPinningSource (not isPinning))}
	</th>
	<th colspan={List.length dates}>
	  <a class="glyphicon glyphicon_chevron_left" onclick=
	  {fn _ =>
	      let val count = List.length dates in
		  case dates of
		      start :: _ => timeSheet <- rpc (timeSheetModel userId
								     count
								     (addDays (0 - count) start)); set timeSheetSource (Some timeSheet)
		    | _ => return ()
	      end}/>
	    
	    Date
	    
	    <a class="glyphicon glyphicon_chevron_right" onclick=
	    {fn _ =>
		let val count = List.length dates in
		    case dates of
			start :: _ => timeSheet <- rpc (timeSheetModel userId
								       count
								       (addDays count start)); set timeSheetSource (Some timeSheet)
		      | _ => return ()
		end}/>
	      
	      <span class="pull_right">
		{let val count = List.length dates in
		     if count > 1 then
			 <xml>
			   <a class="glyphicon glyphicon_minus_sign" onclick=
			   {fn _ =>
			       case dates of
				   start :: _ :: _ => timeSheet <- rpc (timeSheetModel userId
										       (count - 1)
										       start); set timeSheetSource (Some timeSheet)
				 | _ => return ()}/>			   
			 </xml>
		     else
			 <xml>
			 </xml>
		 end}
		  
		  <a class="glyphicon glyphicon_plus_sign" onclick=
		  {fn _ =>
		      let val count = List.length dates in
			  case dates of
			      start :: _ => timeSheet <- rpc (timeSheetModel userId
									     (count + 1)
									     start); set timeSheetSource (Some timeSheet)
			    | _ => return ()
				   
		      end}/>
		  </span>
                </th>
              </tr>
	      <tr>
		<th>Project</th>
		<th>Task</th>
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
        </xml>}/>
	       
        </div>
      </div>
    </div>
  </body>
</xml>

fun main () =
    timeSheetView 1 None None
