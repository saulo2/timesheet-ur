open Bootstrap

fun application () =
    s <- source None;
    let val onload =
	    start <- now;
	    v <- Model.timeSheetModel 1 start 7;
	    set s (Some v)

	val signal =
	    o <- signal s;
	    case o of
		Some v => View.timeSheetView v
	      | None => return <xml></xml>
    in
	return 
	    <xml>
	      <head>
		<link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"/>
		<link rel="stylesheet" type="text/css" href="/TimeSheet/timeSheet.css"/>
	      </head>
	      <body onload={onload}>
		<div class="container">
		  <div class="row">
		    <div class="col-sm-12">
		      <dyn signal={signal}/>
		    </div>
		  </div>
		</div>
	      </body>
	    </xml>
    end
