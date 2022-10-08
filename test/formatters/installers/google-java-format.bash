apt-get install -y default-jre

ver="$(latest_release google/google-java-format | sed 's/^v//')"

mkdir /opt/google-java-format
wget "https://github.com/google/google-java-format/releases/download/v${ver}/google-java-format-${ver}-all-deps.jar" -O /opt/google-java-format/google-java-format.jar

cat <<"EOF" > /usr/local/bin/google-java-format
#!/bin/sh
exec java -jar /opt/google-java-format/google-java-format.jar "$@"
EOF
chmod +x /usr/local/bin/google-java-format
