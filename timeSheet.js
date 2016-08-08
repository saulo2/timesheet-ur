(function () {
    "use strict"

    var rpcCountSource

    window.setUp = function () {
	rpcCountSource = sc (0)
	
	var requestUri = window.requestUri
	window.requestUri = function (xhr, uri, needsSig, isRpc) {
            var rpcCount = sg (rpcCountSource)
            sv (rpcCountSource, rpcCount + 1)
            requestUri (xhr, uri, needsSig, isRpc)
	}

	var xhrFinished = window.xhrFinished
	window.xhrFinished = function (xhr) {
            var rpcCount = sg (rpcCountSource)
            sv (rpcCountSource, rpcCount - 1)
            xhrFinished (xhr)
	}	
    }

    window.rpcCountSource = function () {
	return rpcCountSource
    }

    window.blur = function (id) {
        var element = document.getElementById(id)
        if (element) {
            element.blur()
        }
    }
})()
