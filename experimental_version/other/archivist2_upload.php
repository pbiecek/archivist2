<?php
$target_dir = "archivist2/";
$target_file = $target_dir . basename($_FILES["fileToUpload"]["name"]);
$uploadOk = 1;
$imageFileType = pathinfo($target_file,PATHINFO_EXTENSION);
// If it's spam do nothing
if($_POST["submit"] == "787878") {
// Check if file already exists
	if (file_exists($target_file)) {
	    echo "Sorry, file already exists.";
	    $uploadOk = 0;
	} else{
	// Check file size
		if ($_FILES["fileToUpload"]["size"] > 10000) {
		    echo "Sorry, your file is too large.";
		    $uploadOk = 0;
		} else {
		// Allow certain file formats
			if($imageFileType != "rda" ) {
			    echo "Sorry, only RDA files are allowed.";
			    $uploadOk = 0;
			} 
		}
	}
	// Check if $uploadOk is set to 0 by an error
	if ($uploadOk == 0) {
	//    echo "Sorry, your file was not uploaded.";
	// if everything is ok, try to upload file
	} else {
	    if (move_uploaded_file($_FILES["fileToUpload"]["tmp_name"], $target_file)) {
	        echo "File uploaded.";

			$username = "root";
			$password = "archivist2_787878";
			$hostname = "localhost"; 
			$mysqli = new mysqli($hostname, $username, $password, "archivist2");
			if (mysqli_connect_errno()) {
			    printf("Connect failed: %s\n", mysqli_connect_error());
			} else {
	    	    $sqlquery = "INSERT INTO artifact values('".
			    $mysqli->real_escape_string($_POST["md5hash"])."','".
			    $mysqli->real_escape_string($_POST["name"])."','".
			    $mysqli->real_escape_string($_POST["createdDate"])."') ";
			    $mysqli->query($sqlquery);
		//	echo($sqlquery);
			    $tags = explode("\n", $_POST["tags"]);
			    foreach ($tags as $tag) {
		    		$sqlquery = "INSERT INTO tag values('".
					    $mysqli->real_escape_string($_POST["md5hash"])."','".
					    $mysqli->real_escape_string($tag)."','".
					    $mysqli->real_escape_string($_POST["createdDate"])."') ";
		//	echo($sqlquery);
					$mysqli->query($sqlquery);
			    }
			    echo " Database updated. ";
			    mysql_close($dbhandle);
			}
	    } else {
	        echo "Sorry, there was an error uploading your file.";
	    }
	}
}
?>
