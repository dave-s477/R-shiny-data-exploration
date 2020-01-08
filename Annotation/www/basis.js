$( document ).ready(function() {
    //Click events for the Helptext Button
    $('.help-text-btn').click(function(){
      let $text = $(this).parent().children( ".help-text" );
      if($text.hasClass("hidden")) {
        $text.removeClass("hidden");
        $(this).html('<i class="fa fa-times"></i>');
      }
      else {
        $text.addClass("hidden");
        $(this).html('<i class="fa fa-info"></i>');
      }
    });
    

    //Update the Url if a hash is presented (#shiny-tab-three -> ?tab=three)
    if(window.location.hash) {
      pieces = window.location.hash.split("-");
      query = {};
      query.tab = pieces[pieces.length-1];
      updateURL(query);
    }
    
    //Update the Url on tab click (#shiny-tab-three -> ?tab=three)
    $('body').on('click', '#menu li a', function () {
      pieces = $(this).prop("hash").split("-");
      query = {};
      query.tab = pieces[pieces.length-1];
      updateURL(query);
    });
});


//Track user input changes
var initValues = {};
var query = {};

//Get URL Params from Site request
window.location.search.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(str,key,value) {
    query[key] = decodeURIComponent(value);
  }
);

//Track all Changes from Inputs
$(document).on('shiny:inputchanged', function(event) {
  //Filter messages 
  if(!event.name.includes(".clientdata_output") && event.name.includes("-") && event.el) {
    if(event.el.tagName !== ("BUTTON")) {
      addToQuery(event);
    }
  }
});

$(document).on('click', "input[value='show']", function () {
  //alert("Hide Click");
});

function addToQuery(event) {
  pieces = event.name.split("-");
  name = pieces[(pieces.length)-1];
  if(name in initValues) {
    if((initValues[name].toString() != event.value.toString())) {
      query[name] = event.value;
      updateURL (query);
      console.log("New Value: " + name + ", Values: " + initValues[name] + " -> " + event.value);
    }
    else if (name in query && query[name].toString() != event.value.toString()) {
      delete query[name];
      updateURL (query);
    }
  }
  else {
    initValues[name] = event.value;
    console.log("Init Value: " + name);
  }
}


function updateURL (query) {
    var newurl = window.location.protocol + "//" + window.location.host + window.location.pathname + '?';
    for (var key in query) {
      let param = "";
      if (query.hasOwnProperty(key)) {
        if(query[key] === null) {
          param = key + "=";
        }
        else {
          param = key + "=" + encodeURIComponent(query[key].toString());
        }
      }
      newurl += param + "&";
    }
    window.history.pushState({path:newurl}, "Inside Explorer", newurl.slice(0, -1));
}
