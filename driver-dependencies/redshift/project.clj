(defproject metabase.dependencies.redshift "1.2.18.1036"
  :dependencies [[com.amazon.redshift/redshift-jdbc42-no-awssdk "1.2.18.1036"]]
  :repositories [["redshift" "https://s3.amazonaws.com/redshift-maven-repository/release"]]
  :target-path  "target/%s"
  :uberjar-name "metabase.dependencies.redshift.jar")
