<?php

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
	if ($input) echo $input; else echo ltrim($_SERVER["PATH_INFO"], "/");
}

//phpinfo();