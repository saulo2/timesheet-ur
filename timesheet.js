(function () {
    "use strict"

    window.blur = function (id) {
        var element = document.getElementById(id)
        if (element) {
            element.blur()
        }
    }
})()
