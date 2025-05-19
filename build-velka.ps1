$projects = @("velka.util", "velka.types", "velka.java", "velka.core", "velka.clojure", "velka.parser", "velka.java.generate", "velka.compiler")

foreach($project in $projects) {
	echo "Building $project"
	
	$lib = ".\lib\$($project).jar"
	if(Test-Path $lib){
		Remove-Item $lib -Force
	}
	
	cd ".\$project"
	ant $args
	cd ..
	cp (".\$($project)\jar\$($project).jar") $lib
}

echo "VELKA BUILD FINISHED"