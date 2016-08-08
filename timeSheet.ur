open Bootstrap

style busyIndicatorMask
style busyIndicatorContainer  

style fa
style fa_spinner
style fa_5x
style fa_spin

ffi setUp: transaction unit
ffi rpcCountSource: unit -> source int

fun application () =
    s <- source None;
    let val onload =
	    start <- now;
	    v <- Model.timeSheetModel 1 start 7;
	    set s (Some v)

	val timeSheet =
	    o <- signal s;
	    case o of
		Some v => View.timeSheetView v
	      | None => return <xml></xml>

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
		    </div>
		  </div>
		</div>
		<dyn signal={busyIndicator}/>		
	      </body>
	    </xml>
    end
