<?php
/*
Echo Server for Testing

Launch:

php --server localhost:8888 index.php

Usage:

By default input (put/post/etc data) is echoed, or if empty, the path is echoed:

curl --data abc http://localhost:8888/ --> abc
curl --upload-file abcfile http://localhost:8888/ --> abc
curl http://localhost:8888/abc --> abc

PHP $_SERVER or $_REQUEST (post/get params) values are echoed,
if query param matches one of their keys:

curl http://localhost:8888/?REQUEST_METHOD --> GET
curl http://localhost:8888/?HTTP_USER_AGENT --> curl
curl --data foo=bar http://localhost:8888/?foo --> bar
curl http://localhost:8888/?foo=bar --> bar

Response code can be set with the r parameter:

$ curl --include http://localhost:8888/abc?r=404
HTTP/1.1 404 Not Found
...
abc

Notes:

Due to PHP limitations, input isn't echoed for multipart/form-data,
So an empty string is output. However, individual fields can be requested:

curl --form key=abc http://localhost:8888/ --> ""
curl --form key=abc http://localhost:8888/?key --> abc

*/

header("Content-Type: text/plain");

// Set custom response code, if requested
if (isset($_GET["r"])) {
	http_response_code($_GET["r"]);
	unset($_GET["r"]);
}

// Output server/request values, if requested
if (count($_GET)) {
	foreach ($_GET as $key => $value) {
		echo isset($_SERVER[$key]) ? $_SERVER[$key] : $_REQUEST[$key];
	}
} else {
	// echo non-empty input, or path
	$input = file_get_contents('php://input');
	$path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
	if ($input) echo $input; else echo ltrim($path, "/");
}

//phpinfo(INFO_VARIABLES);
