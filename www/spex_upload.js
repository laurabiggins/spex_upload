
var modal;


// display modals if info button is clicked
$('#dataset_name_info').click(function () {
  modal = $('#datasetnamemodal')[0];
  modal.style.display = "block";
});

$('#data_filepath_info').click(function () {
  modal = $('#datafilepathmodal')[0];
  modal.style.display = "block";
});

$('#metadata_filepath_info').click(function () {
  modal = $('#metadatafilepathmodal')[0];
  modal.style.display = "block";
});

$('#citation_info').click(function () {
  modal = $('#citationmodal')[0];
  modal.style.display = "block";
});

$('#summary_info').click(function () {
  modal = $('#summaryinfomodal')[0];
  modal.style.display = "block";
});

$('#datatype_info').click(function () {
  modal = $('#datatypemodal')[0];
  modal.style.display = "block";
});


$(".close").click(function() {
  modal.style.display = "none";
})



// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
  if (event.target == modal) {
    modal.style.display = "none";
  }
}


/*
$(document).ready( function () {
    $('#table_id').DataTable();
} );

 var table = $('#example').DataTable( {
    ajax: {
        url: 'https://raw.githubusercontent.com/laurabiggins/spex2/main/arrays.txt',
		//url: 'https://raw.githubusercontent.com/laurabiggins/spex2/main/arrays_single.txt',
        dataSrc: 'data'
    },
	select: true,
	paging: false,
    columns: [
        { data: 'name' },
        { data: 'position' },
		{ data: 'office' },
        { data: 'salary' }
    ]
} );

// https://datatables.net/examples/api/select_row.html
$('#example tbody').on( 'click', 'tr', function () {
	$(this).toggleClass('selected');
} );

var table = $('#example').DataTable();
var selected_row = "";

table.on( 'select', function ( e, dt, type, indexes ) {
    //alert("row selected")
    if ( type === 'row' ) {
        var data = table.rows( indexes ).data();
        //console.log("selected name is :" + data[0].name);
        selected_row = data[0]; // then we can see selected_row from the console
        $('#selected_row_info').text(selected_row.name);
    }
} );


/*$('#example').click(function () {
	alert("table has been clicked")
});
*/

/*$('#example').DataTable( {
    data: data,
    columns: [
        { data: 'name' },
        { data: 'position' },
        { data: 'salary' },
        { data: 'office' }
    ]
} );


var add = function (a, b) {
    return a + b;
};
*/
// create an object with properties and a method
var jedi = {
    name: "Yoda",
    age: 899,
    talk: function () { alert("another... Sky... walk..."); }
};

// can add properties afterwards, can overwrite too
jedi.lightsaber = "purple"  // same dot syntax to access value
