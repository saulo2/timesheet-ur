open Bootstrap

style busyIndicatorMask
style busyIndicatorContainer  
      
style fa
style fa_spinner
style fa_5x
style fa_spin
      
style bar

ffi setUp: transaction unit
ffi rpcCountSource: unit -> source int
	  
val allColors = "#9e0142" :: "#d53e4f" :: "#f46d43" :: "#fdae61" :: "#fee08b" :: "#ffffbf" :: "#e6f598" :: "#abdda4" :: "#66c2a5" :: "#3288bd" :: "#5e4fa2" :: []

fun application () =
    s <- source None;
    let val onload =
	    start <- now;
	    v <- Model.timeSheetModel 1 start 7;
	    set s (Some v)

	val timeSheet =
	    o <- signal s;
	    case o of
		None => return <xml></xml>
	      | Some v => View.timeSheetView v

	val chart =
	    o <- signal s;
	    case o of
		None => return <xml></xml>
	      | Some (s, _, _, _, _, _, _, _) =>
		s <- signal s;
		case s of
		    (_, _, _, []) => return <xml></xml>
		  | (_, _, _, projectRowModels) =>
		    nameTimePairs <- List.mapM (fn (_, name, _, taskRowModels, _) =>
						   time <- List.foldlM (fn (_, _, _, entryCellModels, _) total =>
									   time <- List.foldlM (fn (_, timeSource, _) total =>
												   time <- signal timeSource;
												   let val time = if time = ""
														  then 0.0
														  else readError time in
												       return (total + time)
												   end) 0.0 entryCellModels;
									   return (total + time)) 0.0 taskRowModels;
						   return (name, time)) projectRowModels;

		    let val nameTimePairs = List.filter (fn (_, time) => time > 0.0) nameTimePairs

			val total = List.foldl (fn (_, time) total => total + time) 0.0 nameTimePairs

			val max = List.foldl (fn (_, time) max => if time < max
								  then max
								  else time) 0.0 nameTimePairs

			fun nameTimeColorTuples nameTimePairs colors =
			    case nameTimePairs of
				[] => []
			      | (name, time) :: nameTimePairs =>
				case colors of
				    [] => []
				  | color :: colors =>
				    (name, time, color) :: (nameTimeColorTuples nameTimePairs (case colors of
												   [] => allColors
												 | _ => colors))
				    
			val nameTimeColorTuples = nameTimeColorTuples nameTimePairs allColors
		    in				
			return
			    <xml>
			      <table style="width: 100%">
				<tbody>				
				  {List.mapX (fn (name, time, color) =>
						 let val color = value (property "background-color") (atom color)

						     val height = value (property "height") (atom "100%")

						     val width = 100.0 * time / max
						     val width = show width
						     val width = width ^ "%"
						     val width = value (property "width") (atom width)
								 
						     val s = oneProperty (oneProperty (oneProperty noStyle color) height) width
						 in
						     <xml>
						       <tr>
							 <th style="padding: 5px; text-align: right; white-space: nowrap">
							   {[name]}
							 </th>
							 <td style="padding: 0px; height: 100%; width: 100%">
							   <div style={s}>
							     &nbsp;
							   </div>
							 </td>
						       </tr>
						     </xml>
						 end) nameTimeColorTuples}
				</tbody>
			      </table>
			    </xml>
		    end

        val busyIndicator =
	    rpcCount <- signal (rpcCountSource ());
	    if rpcCount > 0 then
		return
		    <xml>
		      <div class="busyIndicatorMask">
			<div class="busyIndicatorContainer">
			  <i class="fa fa-spinner fa-5x fa-spin"></i>
			</div>
		      </div>
		    </xml>
	    else
		return <xml></xml>
    in
	return 
	    <xml>
	      <head>
		<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"/>
		<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"/>		
		<link rel="stylesheet" type="text/css" href="/TimeSheet/timeSheet.css"/>
		<script code={setUp}/>
	      </head>
	      <body onload={onload}>
		<div class="container">
		  <div class="row">
		    <div class="col-sm-12">
		      <dyn signal={timeSheet}/>
		      
		      <div class="panel panel_default" style="position: fixed; right: 0px; bottom: 0px; margin-bottom: 0px; width: 50vw">
			<div class="panel_heading">
			  <h3 class="panel_title">Time per project</h3>
			</div>
			<div class="panel_body">
			  <dyn signal={chart}/>
			</div>
		      </div>		      		      
		    </div>
		  </div>
		</div>
		<dyn signal={busyIndicator}/>		
	      </body>
	    </xml>
    end
