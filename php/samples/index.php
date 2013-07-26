<?php

include "../src/wroom.php";

$api = new WroomAPI("http://lesbonneschoses.wroom.io/api");

var_dump($api->master());
