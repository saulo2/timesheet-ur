ffi blur: id -> transaction unit

fun entryCellModel (projectId: int) (taskId: int) ((date, time): Service.entryCell): transaction entryCellModel =
    id <- fresh;
    timeSource <- source (show time);
    let fun onkeyup event = if event.KeyCode = 13 then
				time <- get timeSource;
				rpc (Service.saveEntryCell projectId taskId date (readError time));
				blur id
			    else
				return ()
    in
	return (id, timeSource, onkeyup)
    end

fun taskRowModel (projectId: int) ((id, name, visible, entryCells): Service.taskRow): transaction taskRowModel =
    visibleSource <- source visible;
    entryCellModels <- List.mapM (fn entryCell => entryCellModel projectId id entryCell) entryCells;
    let fun toggleVisibility _ =
	    visible <- get visibleSource;
	    let val visible = not visible in
		rpc (Service.saveTaskVisibility projectId id visible);
		set visibleSource visible
	    end
    in
	return (id, name, visibleSource, entryCellModels, toggleVisibility)
    end

fun projectRowModel ((id, name, visible, taskRows): Service.projectRow): transaction projectRowModel =
    visibleSource <- source visible;
    taskRowModels <- List.mapM (fn taskRow => taskRowModel id taskRow) taskRows;
    let fun toggleVisibility _ =
	    visible <- get visibleSource;
	    let val visible = not visible in
		rpc (Service.saveProjectVisibility id visible);
		set visibleSource visible
	    end
    in
	return (id, name, visibleSource, taskRowModels, toggleVisibility)
    end

fun p (userId: int) (start: time) (count: int): transaction (time * int * list time * list projectRowModel) =
    timeSheet <- rpc (Service.timeSheet userId start count);
    case timeSheet of (dates, projectRows) =>
		      projectRowModels <- List.mapM projectRowModel projectRows;
		      return (start, count, dates, projectRowModels)

fun timeSheetModel (userId: int) (start: time) (count: int): transaction timeSheetModel =
    v <- p userId start count;
    s <- source v;
    pinningSource <- source False;
    pinningVisibleSource <- source True;
    let fun goTo start count =
	    v <- p userId start count;
	    set s v

	fun previous _ =
	    v <- get s;
	    case v of (start, count, _, _) => goTo (Service.addDays start (0 - count)) count

	fun next _ =
	    v <- get s;
	    case v of (start, count, _, _) => goTo (Service.addDays start count) count

	fun minus _ =
	    v <- get s;
	    case v of
		(start, count, _, _) => goTo start (count - 1)
	      | (_, 0, _, _) => return ()					

	fun plus _ =
	    v <- get s;
	    case v of
		(start, count, _, _) => goTo start (count + 1)

	fun togglePinning _ =
	    pinning <- get pinningSource;
	    set pinningSource (not pinning)
    in
	return (s, pinningSource, pinningVisibleSource, previous, next, minus, plus, togglePinning)
    end
