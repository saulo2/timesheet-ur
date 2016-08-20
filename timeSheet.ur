open Bootstrap

style busyIndicatorMask
style busyIndicatorContainer  
      
style fa
style fa_spinner
style fa_5x
style fa_spin

style chart

ffi setUp: transaction unit
ffi rpcCountSource: unit -> source int
	  
val allColors = "#9e0142" :: "#d53e4f" :: "#f46d43" :: "#fdae61" :: "#fee08b" :: "#ffffbf" :: "#e6f598" :: "#abdda4" :: "#66c2a5" :: "#3288bd" :: "#5e4fa2" :: []

fun application () =
    s <- source None;

    chartVisibleSource <- source True;
    
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

		    let (* val nameTimePairs = List.filter (fn (_, time) => time > 0.0) nameTimePairs *)

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

			fun toggleChartVisibility e =
			    chartVisible <- get chartVisibleSource;
			    set chartVisibleSource (not chartVisible)

			val signal =
			    chartVisible <- signal chartVisibleSource;
			    if chartVisible then
				return
				    <xml>
				      <div class="panel_body">
					<table>
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
								   <th>
								     {[name]}
								   </th>
								   <td>
								     <div style={s} title={(show time) ^ "h"}>
								       {[time]}h
								     </div>
								   </td>
								 </tr>
							       </xml>
							   end) nameTimeColorTuples}
					  </tbody>
					</table>
				      </div>
				    </xml>
			    else
				return <xml></xml>
		    in
			return
			    <xml>
			      <div class="panel panel_primary chart">
				<div class="panel_heading" onclick={toggleChartVisibility}>
				  <h3 class="panel_title">
				    Time per project
				  </h3>
				</div>
				<dyn signal={signal}/>
			      </div>
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
		<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"/>
		  
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
		      
		      <dyn signal={chart}/>
		    </div>
		  </div>
		</div>
		<dyn signal={busyIndicator}/>		
	      </body>
	    </xml>
    end
