FROM openjdk:8-alpine
COPY target/grep-1.0-SNAPSHOT.jar /usr/local/app/grep/lib/grep.jar
ENTRYPOINT ["java", "-cp", "/usr/local/app/grep/lib/grep.jar", "ca.jrvs.apps.grep.JavaGrepImpl"]

