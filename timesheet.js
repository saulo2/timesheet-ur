(function () {
    "use strict"

    window.setUp = function () {
        var getXHR = window.getXHR 

        window.getXHR = function () {
            var xhr = getXHR()
            console.log(xhr)
            return xhr
        }
    }

    window.blur = function (id) {
        var element = document.getElementById(id)    
        if (element) {
            element.blur()
        }
    }    
})()