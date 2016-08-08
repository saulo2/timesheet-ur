open Bootstrap

fun pushPinButtonView visibleSource activeSource onclick =
    let val dynClass =
	    active <- signal activeSource;
	    return (classes (CLASS "btn btn_sm pull_right")
			    (if active then btn_primary else btn_default))
	    
	val dynStyle =
	    visible <- signal visibleSource;
	    return (if visible
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

fun entryCellView (id, timeSource, onkeyup) =
    <xml>
      <td>
	<ctextbox id={id} source={timeSource} onkeyup={onkeyup}/>
      </td>
    </xml>

fun taskRowView pinningSource (id, name, visibleSource, entryCellModels, toggleVisibility) =
    <xml>
      <th>
	{[name]}	
	<dyn signal={pushPinButtonView pinningSource visibleSource toggleVisibility}/>
      </th>
      {List.mapX entryCellView entryCellModels}
    </xml>

fun projectRowView pinningSource (id, name, visibleSource, taskRowModels, toggleVisibility) =
    pinning <- signal pinningSource;
    visible <- signal visibleSource;
    taskRowModels <- (if pinning then
			  return taskRowModels
		      else
			  List.filterM (fn (_, _, visibleSource, _, _) => signal visibleSource) taskRowModels);
    if pinning || visible then
	return
	    (List.mapXi (fn index taskRow =>
			    <xml>
			      <tr>
				{if index = 0 then
				     <xml>
				       <th rowspan={List.length taskRowModels}>
					 {[name]}
					 <dyn signal={pushPinButtonView pinningSource visibleSource toggleVisibility}/>
				       </th>
				     </xml>
				 else
				     <xml></xml>}
				{taskRowView pinningSource taskRow}
			      </tr>
			    </xml>) taskRowModels)
    else
	return <xml></xml>

fun timeSheetView (s, pinningSource, pinningVisibleSource, previous, next, minus, plus, togglePinning) =
    v <- signal s;
    case v of
	(start, count, dates, projectRows) =>
	return
	    <xml>
	      <table class="css_table table_bordered table_condensed table_responsive table_stripped">
		<thead>
		  <tr>
		    <th colspan=2>
		      <dyn signal={pushPinButtonView pinningVisibleSource pinningSource togglePinning}/>
		    </th>
		    <th colspan={count}>
		      <a class="glyphicon glyphicon_chevron_left" onclick={previous}/>		     
		      Date		      
		      <a class="glyphicon glyphicon_chevron_right" onclick={next}/>		      
		      <span class="pull_right">
			{if count > 1
			 then <xml><a class="glyphicon glyphicon_minus_sign" onclick={minus}/></xml>
			 else <xml></xml>}
			
			<a class="glyphicon glyphicon_plus_sign" onclick={plus}/>
		      </span>
		    </th>
		  </tr>
		  <tr>
		    <th>Project</th>
		    <th>Task</th>
		    {List.mapX (fn date =>
				   <xml>
				     <th>{[timef "%D" date]}</th>
				   </xml>) dates}
		  </tr>
		</thead>
		<tbody>
		  {List.mapX (fn projectRow =>
				 <xml>
				   <dyn signal={projectRowView pinningSource projectRow}/>
				 </xml>) projectRows}
		</tbody>
	      </table>
	    </xml>
