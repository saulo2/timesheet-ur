open Bootstrap

fun pushPinButtonView visibleSource activeSource onclick =
    visible <- signal visibleSource;
    if visible then
	let val dynStyle =
		active <- signal activeSource;
		return (if active
			then STYLE "position: absolute; top: 0px; right: 0px; font-size: 12px; color: #337ab7"
			else STYLE "position: absolute; top: 0px; right: 0px; font-size: 12px; color: #777")
	in
	    return
		<xml>
		  <i class="glyphicon glyphicon_pushpin pull_right" dynStyle={dynStyle} onclick={onclick}/>
		</xml>
	end
    else
	return <xml></xml>

fun entryCellView (id, timeSource, onkeyup) =
    <xml>
      <td>
	<ctextbox id={id} source={timeSource} onkeyup={onkeyup}/>
      </td>
    </xml>

fun taskRowView pinningSource (id, name, visibleSource, entryCellModels, toggleVisibility) =
    <xml>
      <th>
	<div style="position: relative">
	  {[name]}	
	  <dyn signal={pushPinButtonView pinningSource visibleSource toggleVisibility}/>
	</div>
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
					 <div style="position: relative">
					   {[name]}
					   <dyn signal={pushPinButtonView pinningSource visibleSource toggleVisibility}/>
					 </div>
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
		      <div style="position: relative">
			&nbsp;
			<dyn signal={pushPinButtonView pinningVisibleSource pinningSource togglePinning}/>
		      </div>
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
