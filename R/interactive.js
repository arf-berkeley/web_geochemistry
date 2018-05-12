shinyjs.init = function() {
	// alert("Running!")
	// var myTable = $('.dataTable').DataTable();
	// myTable.button().add(0, {action: function() {alert("Yes!")}, text: "Reload"});
	// var myTable = $('.dataTable').DataTable( {
	// 	"oLanguage": {
	// 		"sInfo": ""
	// 	}
	// } );
	// alert("Yes!");
}

shinyjs.scroll = function(param) {
	// var t0 = performance.now();
	var myTable = $('.dataTable').DataTable();
	myTable.row(param).scrollTo(animate=false);
	console.log("Scrolling to " + param);
	// console.log("Scrolling to: " + param)
	// var t1 = performance.now();
	// console.log("Call to doSomething took " + (t1 - t0) + " milliseconds.")
	// alert("test");
}

// https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableRowElement
shinyjs.selected = function() {
	var index;
	var row;
	var myTable = $('.dataTable').DataTable();

	myTable.on('click', 'tr', function() {
		index = this.rowIndex;
		row = myTable.rows().data()[index-1];
		// console.log(index + " " + row);

		Shiny.onInputChange('table_selection', row);
		doument.getElementById('table_selection').value = row;
	});
}