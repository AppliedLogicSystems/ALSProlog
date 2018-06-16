<?php

if (isset($_GET["r"])) {
	http_response_code($_GET["r"]);
	unset($_GET["r"]);
}

if (count($_GET)) {
	foreach ($_GET as $key => $value) {
		echo isset($_SERVER[$key]) ? $_SERVER[$key] : $_REQUEST[$key];
	}
} else {
	echo ltrim($_SERVER["PATH_INFO"], "/");
}

//phpinfo();