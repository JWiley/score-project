$(function(){
	//Handler for basic RPC
	$("#scorebutton").click(function(e){
		e.preventDefault()
		$(".metsssfield").val("")
		var data = [];
		$("tbody tr").each(function(i){
			data[i] = {
				sbp : parseFloat($(this).find(".sbpfield").val()),
				dbp : parseFloat($(this).find(".dbpfield").val()),
				trigs : parseFloat($(this).find(".trigsfield").val()),
				hdl : parseFloat($(this).find(".hdlfield").val()),
				waist : parseFloat($(this).find(".waistfield").val()),
				glucose : parseFloat($(this).find(".glucosefield").val()),
				sex : $(this).find(".sexfield").val()
			};
		});

		//RPC request to score data
		var req = ocpu.rpc("MetSSS", {input : data}, function(output){
			//repopulate the table
			$("tbody tr").each(function(i){
				$(this).find(".sbpfield").val(output[i].sbp);
				$(this).find(".dbpfield").val(output[i].dbp);
				$(this).find(".trigsfield").val(output[i].trigs);
				$(this).find(".hdlfield").val(output[i].hdl);
				$(this).find(".waistfield").val(output[i].waist);
				$(this).find(".glucosefield").val(output[i].glucose);
				$(this).find(".sexfield").val(output[i].sex);
				$(this).find(".metsssfield").val(output[i].metsss);
			});
		}).fail(function(){
			alert(req.responseText);
		});
	});

	//CSV file scoring
	$("#csvfile").on("change", function loadfile(e){
		if(!$("#csvfile").val()) return;
		$("#outputcsv").addClass("hide").attr("href", "");
		$(".spinner").show()
		var req = ocpu.call("MetSSS", {
			input : $("#csvfile")[0].files[0]
		}, function(tmp){
			$("#outputcsv").removeClass("hide").attr("href", tmp.getLoc() + "R/.val/csv")
		}).fail(function(){
			alert(req.responseText)
		}).always(function(){
			$(".spinner").hide()
		});
	});


	//this is just to create a table
	function addrow(){
		$("tbody").append('<tr> <td> <div class="form-group"> <input type="number" min="60" max="300" class="form-control sbpfield" placeholder="SBP"> </div> </td> <td> <div class="form-group"> <input type="number" min="20" max="200" class="form-control dbpfield" placeholder="DBP"> </div> </td> <td> <div class="form-group"> <input type="number" min="0" max="20" class="form-control trigsfield" placeholder="Trigs"> </div> </td> <td> <div class="form-group"> <input type="number" min="0" max="10" class="form-control hdlfield" placeholder="HDL"> </div> </td> <td> <div class="form-group"> <input type="number" min="30" max="200" class="form-control waistfield" placeholder="waist"> </div> </td> <td> <div class="form-group"> <input type="number" min="1" max="50" class="form-control glucosefield" placeholder="glucose"> </div> </td> <td> <div class="form-group"> <select class="form-control sexfield"> <option>Female</option> <option>Male</option> </select> </div> </td> <td> <div class="form-group"> <input disabled="disabled" class="disabled form-control metsssfield"> </div> </td>   </tr>');
	}

	for(var i = 0; i < 5; i++){
		addrow();
	}
});

