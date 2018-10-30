$(document).keyup(function(event) {
    if ($("#login-password").is(":focus") && (event.keyCode == 13)) {
        $("#login-button").click();
    }
});
