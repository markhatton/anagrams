/**
 * solves anagrams via XHR, requires jQuery
 */
function solve(s, onSuccess, onError) {

    var url = '/solve?s='+s;

    if (typeof(onError)==='undefined') onError = function(xhr){
        console.log("get solutions failed with status: ", xhr.status)
    };

    $.ajax({
        url: url,
        type: 'GET',
        success: onSuccess,
        error: onError,
        headers: { Accept: "application/json" },
        dataType: "json"
    });
}

function escape(s) {
    return $('<div/>').text(s).html();
}
